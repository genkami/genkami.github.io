---
layout: post
title: コラッツの予想
tags:
- Haskell
use_mathjax: true
---

過去のブログ(現在閉鎖済み)を供養していた所，ちょっとおもしろい問題が出てきたので掲載します．

コラッツ列とは，与えられたnから始まり以下のようなルールで定義される数列です．

```haskell
collatz 1 = 1 : []
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)
```

実際に実行してみましょう．

```haskell
Prelude> collatz 1
[1]
Prelude> collatz 5
[5,16,8,4,2,1]
Prelude> collatz 10
[10,5,16,8,4,2,1]
Prelude> collatz 100
[100,50,25,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
```

このように，試してみた範囲については，どの数字から始めても最後の要素が1になり終了することがわかります．

実はこれが任意のnに対して成り立つかどうかは証明されておらず，コラッツの問題，もしくはコラッツの予想という名前で知られています．

[コラッツの問題 - Wikipedia](https://ja.wikipedia.org/wiki/%E3%82%B3%E3%83%A9%E3%83%83%E3%83%84%E3%81%AE%E5%95%8F%E9%A1%8C)

この予想に関連した問題として，以下のようなものがあります．

[Problem 14 - Project Euler](https://projecteuler.net/problem=14)

要するに，100万以下の数字のうち，その数字から始まるコラッツ列が一番長くなるものを見つけろという問題です．

この問題はどうも昔(高2くらいのとき，だいたい5年くらい前です)解いたことがあるようなのですが，その時のコードは1..1000000のすべての数に対し，そこから始まるコラッツ列を全部持っておいて，最後にその長さを調べるという信じられないほど効率の悪いものだったので，この機会にまともな時間で解けるように書き直しました．

```haskell
-- Collatz.hs
module Main where

import Data.Function (on)
import Data.Array as Array
import Data.Array ((!))
import Data.List as List

type Cache = Array Integer Integer

makeCache :: Integer -> Cache
makeCache maxIndex = cache
  where
    cache = Array.array (1, maxIndex) [(i, collatz cache i) | i <- [1..maxIndex]]

collatz :: Cache -> Integer -> Integer
collatz _ 1 = 1
collatz cache n
  | even n = 1 + next (n `div` 2)
  | otherwise = 1 + next (3 * n + 1)
  where
    (_, maxN) = Array.bounds cache
    next n' | n' <= maxN = cache ! n'
            | otherwise = collatz cache n'

main :: IO ()
main = print
  $ fst
  $ List.maximumBy (compare `on` snd)
  $ map (\i -> (i, collatz cache i)) [1..1000000]
  where cache = makeCache 1000000
```

メモ化しておくのがぱっと思いつくやり方ですが，コラッツ列の要素の上限がどれくらいになるのか検討がつきにくい(たぶんかなり大きくなるはず)ので，適当な上限を設定してそれ以下の数字から始まるコラッツ列の長さのみメモ化することにしました．

(というか，任意のnについて簡単にnから始まるコラッツ列の要素の上限がわかってしまえばコラッツの予想が証明できてしまいそうなので，これは多分無理なんでしょう)


実行結果:

```sh
$ stack ghc Collatz.hs
[1 of 1] Compiling Main             ( Collatz.hs, Collatz.o )
Linking Collatz ...
$ time ./Collatz
837799

real	0m2.399s
user	0m2.125s
sys	0m0.222s
```

2秒ならまあ上々でしょう．

ググって上の方に出てきたやつと答えが一致してるので，たぶん合ってるはずです．

+ [Project Euler Q14 【最長のコラッツ数列】 - Qiita](https://qiita.com/tea63/items/70e884c4c8a3e6f25dca)
+ [Project Euler 解答](http://kingyojima.net/pje/014.html)
