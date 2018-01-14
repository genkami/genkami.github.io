---
layout: post
title: モナドから始めない継続入門
tags:
- Haskell
---

Twitterのどこかで「Contモナドを使わない継続の説明を書いてくれ」みたいな話を目にしたので書きました．

この記事は，以下のような方を対象に書かれています．

+ Haskellはなんとなく使える．モナドもなんとなくわかる．
+ 息をするように新しいモナドを定義したりはできない．
+ 継続が何かは全く分からない．Schemeとかを使ったこともない．


## 継続とは

Haskell(に限らずほとんどの言語)では，以下のようなステップでプログラムを実行していきます．

1. 何らかの関数に引数を与えて，その計算結果を受け取る．
2. 受け取った計算結果を別な関数の引数に与える．
3. 以下繰り返し．

例えばHaskellで，与えられた3つの数の平均を取る関数を(非常に冗長に)書いてみると，以下のようになるでしょう．

```haskell
module Cont where

import Prelude hiding (div)

add :: Num a => a -> a -> a
add x y = x + y

div :: Fractional a => a -> a -> a
div x y = x / y

average3 :: Fractional a => a -> a -> a -> a
average3 x y z =
  let xPlusY = add x y
      xPlusYPlusZ = add xPlusY z
      average = div xPlusYPlusZ 3
  in average
```

これを実行すると，以下のようになります．

```haskell
*Cont> :l Cont
[1 of 1] Compiling Cont             ( Cont.hs, interpreted )
Ok, modules loaded: Cont.
*Cont> average3 5 4 3
4.0
```

このような通常の書き方を「直接スタイル」といいます．

直接スタイルでは，最初に説明したとおり「何らかの関数に引数を与えて，その計算結果を受け取る」のが基本の操作になります．

これに対して，関数が「その関数自体の実行が終わった後の残りの処理」を受け取り，それに明示的に計算結果を渡していくような書き方を「継続渡しスタイル」といいます．

## 継続渡しスタイルの例

以上のような説明だけではよくわからないかもしれませんが，実は皆さんもどこかで使ったことがあるかもしれません．例えば，JQueryの[`$.get`](http://js.studio-kingdom.com/jquery/ajax/get)では，

```javascript
$.get(
  '/path/to/some/resources',
  {},
  (data) => {
    // GETリクエストが成功したときの処理．
    // data にレスポンスが入る．
});
```

というように，`$.get`の第3引数に「リクエストが終わった後の処理」を渡します．この方法が，継続渡しスタイルに他なりません．

## 継続渡しスタイルの書き方

それでは，先ほど作った`average3`を，継続渡しに書き換えてみましょう．以降では，関数`hoge`の継続渡しバージョンを`hogeCPS`という名前で定義します．

Haskellにおいて，「関数の実行が終わった後の残りの処理」は，それ自体一つの関数で表すことができます．

まずは例として，`add`を継続渡しスタイルで書いてみましょう．

```haskell
module Cont where

...

addCPS :: Num a => a -> a -> (a -> result) -> result
addCPS x y cont = cont (x + y)
```

この定義では，`cont`が残りの処理全体になります．`cont`は，`add`の結果である`x`と`y`の和を受け取って，何らかの型`result`の値を返す関数になるでしょう．

この`addCPS`を実際にインタプリタ上で使ってみましょう．

```haskell
*Cont> add 1 2
3
*Cont> addCPS 1 2

<interactive>:29:1: error:
    • No instance for (Show ((a0 -> result0) -> result0))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
```

このように，`addCPS 1 2`自体は，残りの処理全体を受け取ってそれに`1 + 2`を渡す関数となります．この「残りの処理全体」はどんなものでも大丈夫なので，試しに計算結果を文字列に変換する継続を与えてみましょう．

```haskell
*Cont> addCPS 1 2 $ \sum -> show sum
"3"
```

`$`以降では，`1 + 2`の結果を受け取って，それを文字列に変換しています．このように，計算結果を直接受け取るのではなく，残りの処理を関数に渡してあげるのが継続渡しスタイルでした．

これで雰囲気はわかったと思うので，同様に`div`も継続渡しスタイルに変換していきます．

```haskell
module Cont where

...

divCPS :: Fractional a => a -> a -> (a -> result) -> result
divCPS x y cont = cont (x / y)
```

こちらも試しに適当な継続を与えて実行してみましょう．

```haskell
-- 結果を5回繰り返す継続
*Cont> divCPS 4 2 $ \d -> [d, d, d, d, d]
[2.0,2.0,2.0,2.0,2.0]
```

最後に，`average3CPS`を`addCPS`と`divCPS`を使って書いていきますが，その前に`average3`の定義を見直してみましょう．

```haskell
average3 :: Fractional a => a -> a -> a -> a
average3 x y z =
  let xPlusY = add x y
      xPlusYPlusZ = add xPlusY z
      average = div xPlusYPlusZ 3
  in average
```

`average3`とその周辺では，以下のような処理を行っています．

1. `add x y`の結果を`xPlusY`に束縛し，
2. `add xPlusY z`の結果を`xPlusYPlusZ`に束縛し，
3. `div xPlusYPlusZ 3`の結果を`average`に束縛し，
4. `average`を返し，
5. `average3`が返した値を使って，呼び出し側が何かする

順を追って見ていくと，1の実行時点での継続は2, 3, 4, 5となり，2の実行時点での継続は3, 4, 5となり，3の実行時点での継続は4, 5となり，4の実行時点での継続は5となることがわかります．

したがって，1の実行時点での継続を考えると，`average3CPS`は以下のような形をしているはずです．

```haskell
average3CPS x y z cont =
  addCPS x y $ \xPlusY ->
  ...

```

この「...」の部分には，2, 3, 4, 5の処理が入るはずです．順を追って見ていきましょう．


```haskell
average3CPS :: Fractional a => a -> a -> (a -> result) -> result
average3CPS x y z cont =
  addCPS x y $ \xPlusY ->
  addCPS xPlusY z $ \xPlusYPlusZ ->
  ...
```

2の処理までを実装した段階です．この時点で「...」には，3, 4, 5の処理が入ります．すなわち，3の部分までの処理を書くと，以下のような形になるはずです．

```haskell
average3CPS x y z cont =
  addCPS x y $ \xPlusY ->
  addCPS xPlusY z $ \xPlusYPlusZ ->
  divCPS xPlusYPlusZ 3 $ \average ->
  ...
```

これで`x, y, z`の平均を受け取るところまでは実装できました．最後にやらなければならないことは，もちろん残りの継続`cont`に計算結果を渡してやることです．

というわけで，以下が`average3CPS`の最終的な実装となります．

```haskell
module Cont where

...

average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
average3CPS x y z cont =
  addCPS x y $ \xPlusY ->
  addCPS xPlusY z $ \xPlusYPlusZ ->
  divCPS xPlusYPlusZ 3 $ \average ->
  cont average
```

実際に実行して，`average3CPS`が正しく動くことを確認してみましょう．

```haskell
Prelude> :l Cont
[1 of 1] Compiling Cont             ( Cont.hs, interpreted )
Ok, modules loaded: Cont.
*Cont> average3CPS 3 5 7 $ \avg -> show avg
"5.0"
*Cont> average3CPS 3 5 7 $ \avg -> avg -- もちろん，「何もしない」という継続を与えてもOK
5.0
```

おめでとうございます．これで無事，`average3CPS`を実装することができました．これさえ理解できれば，もっと複雑な継続を使った処理も順を追って理解できるはずです．

## 継続のメリット

継続渡しスタイルを使うことのメリットに，「残りの処理の実行を関数側が制御できる」ことがあります．関数から帰った後の「残りの処理」はあくまでもその関数自体の引数なので，呼ぶも呼ばないも自由ですし，呼んだ後に適当に結果に細工をすることだってできてしまいます．

例えば，ゼロ除算の場合は残りの処理を実行せずに，エラーメッセージを返すような関数`safeDivCPS`を以下のように定義することができます．

```haskell
module Cont where

...

type ErrorMsg = String
safeDivCPS :: (Eq a, Fractional a) =>
  a -> a -> (a -> Either ErrorMsg result) -> Either ErrorMsg result
safeDivCPS _ 0 _ = Left "division by zero"
safeDivCPS x y cont = cont (x / y)
```

都合上継続が`result`ではなく`Either ErrorMsg result`になってしまっていますが，とりあえず今は考えないことにしましょう．(まともに説明しようとすると`ContT`モナド変換子が出てきますが，今回の趣旨から外れるため割愛)

これに適当な継続を与えて，実行してみましょう．継続が`Either ErrorMsg result`の型を返さなければならないのに注意です．

```haskell
*Cont> :l Cont
[1 of 1] Compiling Cont             ( Cont.hs, interpreted )
Ok, modules loaded: Cont.
*Cont> safeDivCPS 3 2 $ \d -> Right ("the answer is " ++ show d)
Right "the answer is 1.5"
*Cont> safeDivCPS 3 0 $ \d -> Right ("the answer is " ++ show d)
Left "division by zero"
```

ゼロ除算の場合は残りの処理を実行せず，直ちに`Left "division by zero"`を返すようになりました．


このように，関数の実行後の振る舞いも含めて制御できてしまうのが，継続のメリットになります．これを利用することで，例えば以下の記事のように，Haskellでループからbreakするといったこともできてしまいます．

[Haskellでループのbreak - Qiita](https://qiita.com/tanakh/items/55441a27f1df878b853e)


今回は継続渡しスタイルそのものの説明でした．長くなったので続きは以下に分割しました．

[Contモナドで継続の便利さを理解する](/2018/01/15/01-haskell-cont-monad.md)
