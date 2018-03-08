---
layout: post
title: Haskellで二重数を使って自動微分
tags:
- Haskell
---

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}

module Dual where

data Dual a = !a :+: !a
  deriving (Show, Read, Eq, Ord)

infixl 6 :+:

instance Floating a => Num (Dual a) where
  (a :+: b) + (a' :+: b') = (a + a') :+: (b + b')
  (a :+: b) - (a' :+: b') = (a - a') :+: (b - b')
  (a :+: b) * (a' :+: b') = (a * a') :+: (a * b' + a' * b)
  fromInteger n = fromInteger n :+: 0
  -- 以下2つはあまり意味のないメソッドだが，
  -- Numに含まれているので実装しなければならない💩
  abs (a :+: b) = sqrt (a ** 2 + b ** 2) :+: 0
  signum (a :+: b) = (a / z) :+: (b / z)
    where z :+: _ = abs (a :+: b)

instance (Floating a, Real a) => Real (Dual a) where
  toRational (a :+: _) = toRational a

instance Floating a => Fractional (Dual a) where
  (a :+: b) / (a' :+: b') = (a / a') :+: (a' * b + a * b') / (a' * a')
  fromRational a = fromRational a :+: 0

instance Floating a => Floating (Dual a) where
  pi = pi :+: 0
  exp (x :+: y) = (exp x :+: 0) * (1 :+: y)
  sin (x :+: y) = sin x :+: y * cos x
  cos (x :+: y) = cos x :+: (-y) * sin x
  log (x :+: y) = log x :+: (y / x)

type Analytical a = (Real a, Floating a)

toDual :: Num a => a -> Dual a
toDual x = x :+: 0

fromDual :: Num a => Dual a -> a
fromDual (x :+: _) = x

ε :: Num a => Dual a
ε = 0 :+: 1

derivDual :: Floating a => (Dual a -> Dual a) -> Dual a -> Dual a
derivDual f x = d :+: 0
  where _ :+: d = f (x + ε) - f x

deriv :: Analytical a => (forall b. Analytical b => b -> b) -> a -> a
deriv f x = fromDual $ derivDual f $ toDual x
```
