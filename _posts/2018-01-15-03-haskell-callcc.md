---
layout: post
title: Contモナドとcall/cc
tags:
- Haskell
---

前回:

+ [モナドから始めない継続入門]({% post_url 2018-01-14-01-haskell-continuation %})
+ [Contモナドを実装する]({% post_url 2018-01-15-01-haskell-cont-monad %})

前回で継続の説明と継続渡しを暗黙的に行う`Cont`モナドが作れたので，今回は`call/cc`の実装を行います．

## call/ccとは

継続の代名詞であるSchemeには，`call-with-current-continuation`略して`call/cc`という関数があります．この関数は「引数として渡された関数に，現在の継続を渡して実行する」という関数です．前回定義した`Cont`モナドを使った例で言うと，次のような雰囲気の関数になります．

```haskell
someAction :: Cont result a
someAction = do
  x <- callCC $ \cont -> do
    -- cont は，... 以降の計算を表す継続になる
    OOO
    XXX
  ...
```

ここで，例えば内側の`do`ブロックの内部で`cont hoge`などと実行すると，`callCC`の呼び出し時点での継続に`hoge`が引数として与えられます．すなわち，`cont hoge`が実行された時点で`someAction`の1行目に戻り，`x`の値が`hoge`で束縛された状態で，`...`以降の実行へ移ります．

また，内側の`do`ブロックの内部で`cont`が一度も呼ばれずに`return`した場合，返ってきた値がそのまま`x`に束縛されて`...`以降の実行へ移ります．

## 実装

先ほど書いたとおり，この`callCC`の使い方は以下のようになります．

```haskell
x <- callCC (\cont -> ...)
```

ここで，`x`の型が`a`であり，最終的な実行結果の型が`result`であるとすると，先ほどの説明によりそれぞれのコード片の型は以下のようになります．

+ `x :: a`
+ `callCC (\cont -> ...) :: Cont result a`
+ `cont :: a -> 何か`
+ `(\cont -> ...)の戻り値 :: Cont result a`

`cont`はこれまでの意味だと「最終的な実行結果までの計算」を表すため`a -> result`でした．しかし，これをそのまま使うとこの`cont`を`Cont`モナドの中に埋め込めなくなってしまいます．そこで，この文脈では少し型をいじって，`a -> Cont result b`になるような`cont`を渡すことにしましょう．すると，

+ `cont :: a -> Cont result b`
+ `(\cont -> ...) :: ((a -> Cont result b) -> Cont result a)`

と書くことができます．これらをまとめると，`callCC`の型は以下のようになります．

```haskell
callCC :: ((a -> Cont result b) -> Cont result a) -> Cont result a
```

次に，具体的な実装を考えていきましょう．先ほどの例をもう一度挙げます．

```haskell
someAction :: Cont result a
someAction = do
  x <- callCC $ \cont -> do
    -- cont は，... 以降の計算を表す継続になる
    OOO
    XXX
  ...
```

内側の`do`ブロックの中で`cont`が呼ばれた場合，その時点での継続を破棄して外側の`...`の部分が呼ばれるのでした．外側の部分の継続が`outerCont`という名前で与えられているとすると，この`cont`は次のような式になるはずです．

```haskell
cont = \a -> Cont $ \_innerCont -> outerCont a
```

これを踏まえると，`callCC`の実装は以下のようになります．

```haskell
callCC f = Cont $ \outerCont ->
  runCont (f $ \a -> Cont $ \_innerCont -> outerCont a) outerCont
```

## call/ccを使ってみる

それでは，たった今完成した`callCC`を使ってみましょう．

```haskell
example :: Cont result Int
example = do
  x <- callCC $ \xCont -> do
    xCont 3
    return 4
  y <- callCC $ \yCont ->
    return 5
  return $ x + y
```

この`example`を実行した結果がどのようになるかわかるでしょうか？

1つ目の`callCC`の内側では，`xCont`に`3`を渡しているため，その時点で内側の`do`ブロック内部での継続は破棄され，`x <- ...`の部分の継続に処理が戻ります．それに対して，2つ目の`callCC`の内側では，`yCont`を呼ばずに単に`5`を返しているため，それが通常通り`y`に渡され，`example`の返す値は`3 + 5 == 8`になります．

```haskell
*Cont> :l Cont
[1 of 1] Compiling Cont             ( Cont.hs, interpreted )
Ok, modules loaded: Cont.
*Cont> runCont example id
8
```

慣れていないと分かりにくいかもしれませんが，これを応用すると面白いものが書けます．

## 応用例: ループからbreak

`[Maybe Int]`を受け取り，`Just x`の形の要素だけの和を取る関数`sumJust`を考えてみます．この場合は必要ありませんが，今後の拡張のためこの関数は`Cont`モナドに包まれるように定義します．

```haskell
sumJust :: [Maybe Int] -> Cont result Int
sumJust xs = foldM iter 0 xs
  where
    iter acc (Just x) = return $ x + acc
    iter acc Nothing = return acc
```

実行結果:

```haskell
*Cont> runCont (sumJust [Just 1, Just 2, Just 3, Nothing, Just 4, Nothing, Just 5]) id
15
```

`1 + 2 + 3 + 4 + 5 == 15`です．

次に，この関数を少し書き換えて，最初に`Nothing`が出て来るまでの値の和を求めるようにしましょう．`callCC`を使うと，あたかも`Nothing`が出てきた時にループを`break`するかのように処理を書くことができます．

```haskell
sumTillNothing :: [Maybe Int] -> Cont result Int
sumTillNothing xs = callCC $ \break -> foldM (iter break) 0 xs
  where
    iter _ acc (Just x) = return $ x + acc
    iter break acc Nothing = break acc
```

実行結果:

```haskell
*Cont> runCont (sumTillNothing [Just 1, Just 2, Just 3, Nothing, Just 4, Nothing, Just 5]) id
6
```

このように，継続をうまく扱うことで，トリッキーな処理を実装することができるようになります．

使い所は限られるかもしれませんが，場合によっては素直に書くと複雑になってしまうような処理を簡単に書くことができるかもしれません．
