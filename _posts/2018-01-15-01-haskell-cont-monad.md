---
layout: post
title: Contモナドを実装する
tags:
- Haskell
---

[モナドから始めない継続入門](/2018/01/14/01-haskell-continuation.md)

上の記事では，モナドの登場しない継続の説明を書きましたが，今回はこのとき使った継続を`Cont`モナドというモナドに変換していきます．

前回定義した`average3CPS`の定義を見直してみましょう．この関数は，3つの引数`x, y, z`の平均を継続に渡す関数でした．

```haskell
module Cont where

addCPS :: Num a => a -> a -> (a -> result) -> result
addCPS x y cont = cont (x + y)

divCPS :: Fractional a => a -> a -> (a -> result) -> result
divCPS x y cont = cont (x / y)

average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
average3CPS x y z cont =
  addCPS x y $ \xPlusY ->
  addCPS xPlusY z $ \xPlusYPlusZ ->
  divCPS xPlusYPlusZ 3 $ \average ->
  cont average
```

`average3CPS`の定義を見てみると，`hoge a b c $ \z -> ...`のような形のボイラープレートが繰り返されていることに気づきます．このような構文上のボイラープレートはモナド化のチャンス！

`average3CPS`に現れるそれぞれの`$`を見てみると，その左側は以下のような型になっていることがわかります．

```haskell
($)の左側 :: (a -> result) -> result
```

(※ここでの`a`は`average3CPS`の型定義に出てくるものとは別物です)

このうち，`result`の部分はプログラムの最終的な実行結果を表すため不変で，`a`の部分は例えば`addCPS x y`や`divCPS xPlusYPlusZ 3`などの結果の型になります．

このことから察するに，`Cont`モナドの構成要素は以下のような形になるはずです．

```haskell
newtype Cont result a = Cont {
  runCont :: (a -> result) -> result
}
```

事実として，これが`Cont`モナドの具体的な型になります．

次に`return`の実装を考えてみましょう．`return`は何もしない(`>=>`の単位元となる)ものなので，今までの書き方で言えば以下のような関数に相当するものになるでしょう．

```haskell
identityCPS :: a -> (a -> result) -> result
identityCPS x cont = cont x
```

最後に，`>>=`の実装について考えてみます．`>>=`は，今までの書き方をすれば以下のような関数に相当する型を持つはずです．

```haskell
bindCPS ::
  ((a -> result) -> result) ->
  (b -> ((c -> result) -> result)) ->
  ((c -> result) -> result)
```

一番最後の括弧は外しても変わらないので，外して考えてみます．

```haskell
bindCPS ::
  ((a -> result) -> result) ->
  (a -> ((b -> result) -> result)) ->
  (b -> result) ->
  result
```

このことから考えると，この`bindCPS`は

1. `a`型の結果を継続に渡す関数`xCPS`を受け取り，
2. `a`を引数として取り，結果の型`b`の値を継続に渡す関数`fCPS`を受け取り，
3. `b`型の値を受け取って最終的な計算結果を返す継続を受け取り，
4. 最終的な計算結果を返す


関数であることがわかります．

これをコードにしてみましょう．

```haskell
bindCPS ::
  ((a -> result) -> result) ->
  (a -> ((b -> result) -> result)) ->
  (b -> result) ->
  result
bindCPS xCPS fCPS cont =
  xCPS $ \xVal ->
  fCPS xVal $ \yVal ->
  cont yVal
```

最後に，これらを新しい型`Cont`に合わせて書き換え，モナドのインスタンスにしてしまいましょう．


```haskell
module Cont where

import Control.Monad (liftM, ap)

newtype Cont result a = Cont {
  runCont :: (a -> result) -> result
  }

returnCont :: a -> Cont result a
returnCont a = Cont $ \cont -> cont a

bindCont :: Cont result a -> (a -> Cont result b) -> Cont result b
bindCont (Cont xCPS) f = Cont $ \cont ->
  xCPS $ \xVal ->
  let Cont yCPS = f xVal
  in yCPS $ \yVal ->
    cont yVal

instance Monad (Cont result) where
  return = returnCont
  (>>=) = bindCont

-- 最近のGHCでは，MonadがApplicativeとFunctorのインスタンスでない場合
-- 怒られるようになっているので，一応これらも定義．
instance Applicative (Cont result) where
  pure = return
  (<*>) = ap

instance Functor (Cont result) where
  fmap = liftM
```

これを用いて，今までの`average3CPS`相当のものを実装していきましょう．

```haskell
module Cont where

...

addCont :: Num a => a -> a -> Cont result a
addCont x y = return $ x + y

divCont :: Fractional a => a -> a -> Cont result a
divCont x y = return $ x / y

average3Cont :: Fractional a => a -> a -> a -> Cont result a
average3Cont x y z = do
  xPlusY <- addCont x y
  xPlusYPlusZ <- addCont xPlusY z
  let average = xPlusYPlusZ / 3
  return average
```

実行は以下のように行います．

```haskell
Prelude> :l Cont
[1 of 1] Compiling Cont             ( Cont.hs, interpreted )
Ok, modules loaded: Cont.
*Cont> runCont (average3Cont 3 4 5) $ \x -> x
4.0
```

継続をモナドにしたメリットは，これまで明示的に`cont`を受け渡ししていたのを，うまく隠蔽することができる所にあります．

```haskell
-- 今までの書き方
average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
average3CPS x y z cont =
  addCPS x y $ \xPlusY ->
  addCPS xPlusY z $ \xPlusYPlusZ ->
  divCPS xPlusYPlusZ 3 $ \average ->
  cont average

-- Contモナドを使った書き方
average3Cont :: Fractional a => a -> a -> a -> Cont result a
average3Cont x y z = do
  xPlusY <- addCont x y
  xPlusYPlusZ <- addCont xPlusY z
  let average = xPlusYPlusZ / 3
  return average
```

今までの書き方では，

```haskell
hogeCPS $ \x -> ...
```

の`...`の部分が，最終的な計算結果を得るまでの残りの処理，すなわち継続を表していましたが，新しい書き方では

```haskell
x <- hogeCont
...
```

の`...`の部分，すなわち，アクションの実行以降の行が暗黙的に継続として扱われます．

これによって，現在の継続をより簡単に扱うことができるようになりました．

次回は現在の継続の取得と，それを用いたちょっとした遊びを書いていきたいと思います．
