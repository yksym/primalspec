primalspec
============

A model language for state machine, or CSP without concurrency and non-determinic transition.

How to use
----------

```
stack build
stack install
prsp sample/vm.csp
```

syntax extension
-----------------

* record

```
datatype Hoge {
    xxx :: Int
}

v :: Hoge
v = Hoge.1

get :: (Hoge) -> Int
get(h) = h ^. xxx

set3 :: (Hoge) -> Hoge
set3(h) = h { xxx = 3 }
```

* global state
```
datatype Global {
    tmp :: Int
}

P = ev1 @( Global.3 ) -> global ^. tmp == 3 & ev2 -> SKIP

assert P [T= ev1 -> ev2 -> SKIP
```


TODO
---

* refactoring
* module system
* multi-byte chars
* rich data type(Set, Tuple)
* record construction
* extensible record
* parametric polymorphism
* model checking tool


