primalspec
============

CSPMベースの原始的な状態遷移モデル記述言語(原始的: 並行合成と非決定性が扱えない)


使い方
----------

```
prsp sample/vm.csp
```


注意点
----------

* 並行合成、内部選択、リネーム、隠蔽等は提供しない
* 詳細化もほぼ未サポート。[T= はあるが、右辺がペイロードの値が特定可能なイベントとPrefix演算子とSKIPのみで構成されている必要がある
* P = P や P = P [] Q のようなPrefix演算子を挟まない再帰的な式を評価するとstack overflow エラーが発生する

独自拡張
----------

T.B.D

* datatypeでレコードも扱える(Haskellのlensライクにアクセスする必要がある)
* 状態を参照する特別なシンボル global
```
global^.hoge == 3 & ev.4 -> ...
```
* 状態の更新はイベントの直後
```
ev.3 @( global {hoge = 3, huga = 4} ) -> ...
```


TODO
---

* module system
* multi-byte chars
* Set
* Tuple
* record construction
* extensible record like?
* param poly


