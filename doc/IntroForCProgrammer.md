Introduction fo C Programmer
=============

3分 で 学ぶHaskell


コメント
-----------

```haskell
-- // 相当。行末までコメントです
```

関数定義
-----------

```haskell
-- シンボル :: 型
-- シンボル = 式

f :: Int -> Int
f x = x + 1
```

関数適用
-----------

```haskell
 f x -- 関数適用。Cだと f(x)
 f $ g x  -- 括弧を省略する為の演算子。Cだと f (g x)
```

ラムダ関数
---------

```haskell
f :: Int -> Int
f = \x -> x + 1
```

構造体の定義
-----------

```haskell
data Foo = Foo {
  _hoge :: Int, -- _が最初につく点に注意
  _huga :: String
}

makeLenses ''Foo -- おまじない
```

構造体のメンバアクセスと変更
----------------------------

```haskell
getHoge :: Foo -> Int
getHoge a = a ^. hoge

setHuga :: Foo -> Int -> String -> Foo
setHuga a n s = a &~ do { hoge .= n; huga .= s }
```

直和型(enum)の定義
----------------------------

```haskell
data Color = Red | Blue | Gray Int
```

直和型のパターンマッチ
----------------------------

```haskell
color2number :: Color -> Int
color2number Red      = 0
color2number Blue     = 1
color2number (Gray n) = 2 + n
```

分岐
----------------------------

```haskell
f :: Int -> Int
f n = if
    | n == 0 -> 3
    | n == 1 -> 4
    | otherwise -> 5
```






