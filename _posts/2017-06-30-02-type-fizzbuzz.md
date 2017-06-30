---
layout: post
title: 型レベルでFizz Buzz
tags:
- Haskell
---

`GHC.TypeList`には`Nat`というkindが定義されており、自然数のリテラルをkindが`Nat`である型として利用することができます。

また、基本的な計算(`+`, `-`, `*`, `~`)や、比較(`==`, `CmpNat`)なども用意されているので、型レベルFizz Buzzくらいなら比較的簡単に書けてしまいます。

``` haskell
-- TypeFizzBuzz.hs
{-# LANGUAGE
    KindSignatures
  , DataKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances #-}
import GHC.TypeLits

type Mod (m :: Nat) (n :: Nat) = Mod' m n (CmpNat m n)
type family Mod' (m :: Nat) (n :: Nat) (ord :: Ordering) :: Nat where
  Mod' _ _ EQ = 0
  Mod' m n GT = Mod (m - n) n
  Mod' m _ LT = m

data FizzBuzz = Fizz | Buzz | FizzBuzz | N Nat

type ToFizzBuzz (n :: Nat) = ToFizzBuzz' n (Mod n 3) (Mod n 5)
type family ToFizzBuzz' (n :: Nat) (mod3 :: Nat) (mod5 :: Nat) :: FizzBuzz where
  ToFizzBuzz' _ 0 0 = 'FizzBuzz
  ToFizzBuzz' _ 0 _ = Fizz
  ToFizzBuzz' _ _ 0 = Buzz
  ToFizzBuzz' n _ _ = N n

type IterFizzBuzz (from :: Nat) (to :: Nat) = IterFizzBuzz' from to (CmpNat from to)
type family IterFizzBuzz' (from :: Nat) (to :: Nat) (ord :: Ordering) :: [FizzBuzz] where
  IterFizzBuzz' from to GT = '[]
  IterFizzBuzz' from to _ = ToFizzBuzz from : (IterFizzBuzz (from + 1) to)
```

`IterFizzBuzz n m`が`n`から`m`までの数値に対しての`FizzBuzz`を型レベルのリストにして返す型族になっています。

``` haskell
ghci> :m +Data.Proxy
ghci> :set -XDataKinds
ghci> let p = Proxy :: Proxy (IterFizzBuzz 1 100)
ghci> :t p
p :: Proxy
       '['N 1, 'N 2, 'Fizz, 'N 4, 'Buzz, 'Fizz, 'N 7, 'N 8, 'Fizz, 'Buzz,
         'N 11, 'Fizz, 'N 13, 'N 14, 'FizzBuzz, 'N 16, 'N 17, 'Fizz, 'N 19,
         'Buzz, 'Fizz, 'N 22, 'N 23, 'Fizz, 'Buzz, 'N 26, 'Fizz, 'N 28,
         'N 29, 'FizzBuzz, 'N 31, 'N 32, 'Fizz, 'N 34, 'Buzz, 'Fizz, 'N 37,
         'N 38, 'Fizz, 'Buzz, 'N 41, 'Fizz, 'N 43, 'N 44, 'FizzBuzz, 'N 46,
         'N 47, 'Fizz, 'N 49, 'Buzz, 'Fizz, 'N 52, 'N 53, 'Fizz, 'Buzz,
         'N 56, 'Fizz, 'N 58, 'N 59, 'FizzBuzz, 'N 61, 'N 62, 'Fizz, 'N 64,
         'Buzz, 'Fizz, 'N 67, 'N 68, 'Fizz, 'Buzz, 'N 71, 'Fizz, 'N 73,
         'N 74, 'FizzBuzz, 'N 76, 'N 77, 'Fizz, 'N 79, 'Buzz, 'Fizz, 'N 82,
         'N 83, 'Fizz, 'Buzz, 'N 86, 'Fizz, 'N 88, 'N 89, 'FizzBuzz, 'N 91,
         'N 92, 'Fizz, 'N 94, 'Buzz, 'Fizz, 'N 97, 'N 98, 'Fizz, 'Buzz]
```
