primalspec
============

A model checker for state machine; a lesser [FDR](https://www.cs.ox.ac.uk/projects/fdr/).

* CSP without concurrency and internal event(non-determinic transition)
* MIT License
* command line tool
* syntax extension(record type, global state)


How to use
----------

```
stack build
stack install

prsp sample/vm.csp
```

syntax extension
-----------------

### record

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
### global state

```
datatype Global {
    tmp :: Int
}

-- 'global' is a special symbol, which is typed as 'Global' and represents global state.
-- 'ev @( g )' means that global state is g just after event 'ev' occurs.
P = ev1 @( Global.3 ) -> global ^. tmp == 3 & ev2 -> SKIP

assert P [T= ev1 -> ev2 -> SKIP
```

NOTE: Currently, only '''->''' and ''';''' can be used in a RHS of '''[T='''.
NOTE: This semantics is very confusing, so use only in process expression.



TODO
---

* multi-byte chars comment
* remove buggy [T=, support [:has trace]
* support HM like type system, Set, Seq, Tuple ....
* remove recordtype, use tuple and buildtin lens
* remove context, support [|alphabets|]

* expr inspector / traceShow
* support print state
* support model checking to detect deadlock
* tutorial

* make code elegant
* make event Expr (now event is not used as an argument)
* module system
* event-hook function for programing

* repl checker


