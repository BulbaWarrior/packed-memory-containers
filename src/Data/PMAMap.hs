{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}
module Data.PMAMap where


import           Prelude hiding (lookup)
import qualified Data.PackedMemoryArray as PMA
import           Data.PackedMemoryArray (PMA)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import           Control.Monad.ST (runST)
import qualified Data.Map as DMap


_THRESHOLD :: Int
_THRESHOLD = 10

data Map k a = Map { getNS     :: NS k a
                   , getNsSize :: Int
                   , getPMA    :: PMA k a
                   , getMap    :: DMap.Map k a
                   }
  deriving (Show)

type Chunk k a = Vector (k, a)

-- todo replace with Arrays?
data NS k a = M0
            | M1 !(Vector (k, a))
            | M2 !(Vector (k, a)) !(Vector (k, a)) (Vector (k, a)) !(NS k a)
            | M3 !(Vector (k, a)) !(Vector (k, a)) !(Vector (k, a)) (Vector (k, a)) !(NS k a)

instance (Show k, Show a) => Show (NS k a) where
  show (M0)              = "0 "
  show (M1 as)           = "1 " ++ show as ++ " "
  show (M2 as bs _ m)    = "2 " ++ show as ++ " " ++ show bs ++ " " ++ show m
  show (M3 as bs cs _ m) = "3 " ++ show as ++ " " ++ show bs ++ " " ++ show cs ++ " " ++ show m


null :: Map k v -> Bool
null m = DMap.null (getMap m) && PMA.null (getPMA m) && nullNS (getNS m)
  where
    nullNS M0 = True
    nullNS _  = False

empty :: Map k v
empty = Map { getNS      = M0
            , getNsSize = 0
            , getPMA     = PMA.empty
            , getMap     = DMap.empty
            }

singleton :: k -> v -> Map k v
singleton k v = Map { getNS      = M0
                    , getNsSize = 1
                    , getPMA     = PMA.empty
                    , getMap     = DMap.singleton k v
                    }


lookup :: (Ord k) => k -> Map k v -> Maybe v
lookup !k m = maybe fromNS Just (maybe fromPMA Just fromMap)
  where
    fromMap = DMap.lookup k (getMap m)
    fromPMA = PMA.lookup k (getPMA m)
    fromNS = go (getNS m)

    go M0                = Nothing
    go (M1 as)           = lookup1 k as Nothing
    go (M2 as bs _ rest)    = lookup1 k as $ lookup1 k bs $ go rest
    go (M3 as bs cs _ rest) = lookup1 k as $ lookup1 k bs $ lookup1 k cs $ go rest

lookup1 :: (Ord k) => k -> Chunk k v -> Maybe v -> Maybe v
lookup1 k chunk r
  | Just v <- linSearch k chunk = Just v
  | otherwise              = r
  where
    -- todo bin search 
    linSearch tf vector = snd <$> Vector.find (\(k', _) -> k' == tf) vector


-- todo rewrite with Array? Better solution?
merge :: (Ord k) => Chunk k a -> Chunk k a -> Chunk k a
merge v1 v2 = runST $ do
  mv1 <- Vector.thaw v1
  mv2 <- Vector.thaw v2
  fv <- MVector.new (MVector.length mv1 + MVector.length mv2)
  mergeM mv1 mv2 fv
  Vector.freeze fv
  where
    mergeM mv1 mv2 fv
      | MVector.null mv1 && MVector.null mv2 = return ()
      | MVector.null mv1 = do
        val <- MVector.read mv2 0
        MVector.write fv 0 val
        mergeM mv1 (MVector.tail mv2) (MVector.tail fv)
      | MVector.null mv2 = do
        val <- MVector.read mv1 0
        MVector.write fv 0 val
        mergeM (MVector.tail mv1) mv2 (MVector.tail fv)
      | otherwise = do
        val1 <- MVector.read mv1 0
        val2 <- MVector.read mv2 0
        if fst val1 < fst val2
          then do
            MVector.write fv 0 val1
            mergeM (MVector.tail mv1) mv2 (MVector.tail fv)
          else do
            MVector.write fv 0 val2
            mergeM mv1 (MVector.tail mv2) (MVector.tail fv)

chunkFromMap :: (Ord k) => DMap.Map k v -> Chunk k v
chunkFromMap = Vector.fromList . DMap.assocs

insertP :: (Ord k) => k -> v -> Map k v -> Map k v
insertP k0 v0 m
  | n0 < _THRESHOLD = dumpPMA $ m { getMap  = (DMap.insert k0 v0 (getMap m)) }
  | otherwise       = dumpPMA $ m { getMap = DMap.singleton k0 v0
                                  , getNS  = summ (chunkFromMap (getMap m)) (getNS m)
                                  , getNsSize = getNsSize m + 1
                                  }
 where
  n0 = DMap.size (getMap m)

insertE :: (Ord k) => k -> a -> Map k a -> Map k a
insertE k a m 
  -- todo check if it is still better than the usual insert, and flush accordingly
  | position (PMA.cardinality (getPMA m) + 1) > order (getNS m) = insertE k a $ dumpPMA m
  | otherwise                                                   = newMap
  where
    -- todo rewrite in constant time using math 
    order M0                = 0
    order (M1 _)            = 0
    order (M2 _ _ _ rest)   = order rest + 1
    order (M3 _ _ _ _ rest) = order rest + 1
    newMap = m { getPMA = PMA.insert k a (getPMA m) }


-- todo is it amortized?
dumpPMA :: (Ord k) => Map k a -> Map k a
dumpPMA m
  | len < _THRESHOLD = m
  | otherwise        = m { getNS  = skipModify (position len) (summ $ pmaToVec pma) ns
                         , getPMA = PMA.empty
                         , getNsSize = getNsSize m + 2 ^ (position len)
                         }
  where
    -- todo is it any good
    pmaToVec = Vector.mapMaybe id . PMA.elements 

    ns = getNS m
    pma = getPMA m
    len = PMA.cardinality pma -- todo does the invariant hold

    skipModify 0 f ns'                  = f ns'
    skipModify _ _ M0                   = error "too far skip"
    skipModify _ _ (M1 _)               = error "too far skip"
    skipModify n f (M2 as bs ab's rest)    = M2 as bs ab's (skipModify (n-1) f rest)
    skipModify n f (M3 as bs cs bc's rest) = M3 as bs cs bc's (skipModify (n-1) f rest)

-- todo rename
position :: Int -> Int
position size = floor (logBase (2 :: Double) (fromIntegral (size `div` _THRESHOLD)))

summ :: (Ord k) => Chunk k a -> NS k a -> NS k a
summ as M0                 = M1 as
summ as (M1 bs)            = M2 as bs (merge as bs) M0
summ as (M2 bs cs bcs xs)  = M3 as bs cs bcs xs
summ as (M3 bs _ _ cds xs) = cds `seq` M2 as bs (merge as bs) (summ cds xs)


-- * debug
-- todo delete

ie :: (Show a, Ord a) => a -> Map a String -> Map a String
ie a m = insertE a (show a) m

ip :: (Show a, Ord a) => a -> Map a String -> Map a String
ip a m = insertP a (show a) m


_m :: Map Int String
_m = foldr (\a m -> ip a m) empty [1..10]


_show' :: (Show s) => (Chunk k a -> s) -> NS k a -> String
_show' _ (M0)              = "0 ()"
_show' f (M1 as)           = "1 (" ++ (show . f) as ++ ") "
_show' f (M2 as bs _ m)    = "2 (" ++ (show . f) as ++ ") (" ++ (show . f) bs ++ ") " ++ _show' f m
_show' f (M3 as bs cs _ m) = "3 (" ++ (show . f) as ++ ") (" ++ (show . f) bs ++ ") (" ++ (show . f) cs ++ ") " ++ _show' f m


_show :: (Show s) => (Chunk k a -> s) -> Map k a -> String
_show f m = _show' f (getNS m)

