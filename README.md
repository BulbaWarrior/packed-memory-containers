# sultanov-thesis-2020
Repository with code, text and documentation relevant to Azat Sultanov's thesis for 2020.

## Analysis

Heavy-light decomposition can process some `change` and `get` queries on tree path from `x` to `y`.

Time complexity for `change` operation is `O(log n * f(n))`, where `f(n)` is time complexity for `change` operation for data structure that was used for processing queries on heavy paths.

Time complexity for `get` operation is `O(log n * g(n))`, where `g(n)` is time complexity for `get` operation for data structure that was used for processing queries on heavy paths.

Pros:
* Since `query(x, y)` (some query on path from `x` to `y`) can be splitted into two queries `query(x, lca(x, y))` and `query(y, lca(x, y))` (where `lca(x, y)` is lowest common ancestor of `x` and `y`), query can be easily parallelized.

* Also `get` and `change` queries for different heavy paths are independent, so they can be parallelized, too.

Cons:
* Sensitive to changes of tree structure (for example, adding new leaf can transform some heavy edges to light)

## Applications

### Real projects:
* [A New Perspective on the Tree Edit Distance](https://link.springer.com/chapter/10.1007/978-3-319-68474-1_11)
* [Fault diagnosis in fiber-optic communication networks](https://www.researchgate.net/publication/224710753_Non-Adaptive_Fault_Diagnosis_for_All-Optical_Networks_via_Combinatorial_Group_Testing_on_Graphs)
* [Random Access to Grammar-Compressed Strings and Trees](https://arxiv.org/pdf/1001.1565.pdf)

### Competitive programming problems:

* https://www.spoj.com/problems/QTREE/
* https://codeforces.com/contest/487/problem/E?locale=en
* https://codeforces.com/contest/117/problem/E?locale=en

## Useful links:
* [Chris Okasaki. Purely Functional Data Structures](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
