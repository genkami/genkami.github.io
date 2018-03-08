---
layout: post
title: Haskellに型なんていらないIntegerがあれば十分だ
tags:
- Haskell
---

```haskell
module Nat where

type T = Integer

pair :: T -> T -> T
pair x y = y + ((x + y) * (x + y + 1) `div` 2)

car :: T -> T
car p = iter 0 0 0
  where
    iter k x y
      | x < 0 = iter (k + 1) (k + 1) 0
      | pair x y == p = x
      | otherwise = iter k (x - 1) (y + 1)

cdr :: T -> T
cdr p = iter 0 0 0
  where
    iter k x y
      | x < 0 = iter (k + 1) (k + 1) 0
      | pair x y == p = y
      | otherwise = iter k (x - 1) (y + 1)

nil :: T
nil = 0

cons :: T -> T -> T
cons x xs = pair x xs + 1

-- リスト xs の先頭要素を取得．
-- xs が空リストの場合， 0 を返す．
hd :: T -> T
hd 0 = 0
hd xs = car (xs - 1)

tl :: T -> T
tl 0 = 0
tl xs = cdr (xs - 1)

-- リスト xs の n 番目の値を取得．
-- xs の長さが n 以下だった場合は 0 を返す．
nth :: T -> T -> T
nth 0 _ = 0
nth xs 0 = hd xs
nth xs n = nth (tl xs) (n - 1)

-- リスト xs の長さを取得．
len :: T -> T
len 0 = 0
len xs = 1 + len (tl xs)
```
