# Setup
1. install opam
2. `$ opam switch create . && opam install .`

# How to use

```
$ cat i
d. ($cons *65 ($cons *256 (x. x)))
$ dune exec ./src/main.exe < i > o.lazy
```
`o.lazy` is a LazyK code that prints 'c'.
