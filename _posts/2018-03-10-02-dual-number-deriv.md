---
layout: post
title: Haskellで二重数を使って自動微分
tags:
- Haskell
use_mathjax: true
---

## 二重数とは

複素数の兄弟のようなものに、二重数というものがあります。二重数は実数の集合 \\(\mathbb{R}\\) に新しい元 \\(\epsilon~(\epsilon^2 = 0)\\) を追加したものです。

二重数を用いると、例えば

\\[(x + \epsilon) = x + \epsilon\\]
\\[(x + \epsilon)^2 = x^2 + 2x\epsilon\\]
\\[(x + \epsilon)^3 = x^3 + 3x^2\epsilon\\]
\\[\cdots\\]
\\[(x + \epsilon)^n = x^n + nx^{n-1}\epsilon\\]

というようになり、少なくとも \\(x\\) の多項式 \\(f\\) については

\\[f(x + \epsilon) = x + \epsilon\frac{\mathrm{d}f(x)}{\mathrm{d}x}\\]

となることがわかります。

またテイラー展開を行うことにより、指数関数 \\(e^x\\) についても、

\\[e^{x + b\epsilon} = 1 + \sum_{n=1}^{\infty}\frac{(x + b\epsilon)^n}{n!}\\]
\\[= 1 + \sum_{n=1}^{\infty}\frac{x^n + nx^{n-1}b\epsilon}{n!}\\]
\\[= 1 + \sum_{n=1}^{\infty}\frac{x^n}{n!} + b\epsilon\sum_{n=1}^{\infty}\frac{nx^{n-1}}{n!}\\]
\\[= \left(1 + \sum_{n=1}^{\infty}\frac{x^n}{n!} \right) +  b\epsilon\left(1 + \sum_{n=1}^{\infty}\frac{x^{n}}{n!} \right)\\]
\\[= (1 + b\epsilon)e^x\\]

となるため、先ほどの性質が成り立つことがわかります。

(※二重数に関する性質を完全に理解しているわけではないので上の式変形が合法的に行えるかは分かりませんが、二重数についても\\(e^x\\)のテイラー展開は絶対収束するはずなので大丈夫なはずです)

同様にして、\\(\mathrm{sin}(x), \mathrm{cos}(x)\\)などについても同様の性質を証明することができます。


除法については、分母の実部が\\(0\\)でないときの場合について、

\\[\frac{a + b\epsilon}{c + d\epsilon} = \frac{(a + b\epsilon)(c - d\epsilon)}{(c + d\epsilon)(c - d\epsilon)}\\]
\\[= \frac{ac - \epsilon(ad - bc)}{c^2}\\]
\\[= \frac{a}{c} - \epsilon\frac{ad - bc}{c^2}\\]

と定義します。この定義により、例えば

\\[\frac{1}{(x + \epsilon)^n} = \frac{1}{x^n + nx^{n-1}\epsilon}\\]
\\[= \frac{1}{x^n} - \epsilon\frac{nx^{n-1}}{(x^n)^2}\\]
\\[= \frac{1}{x^n} - \epsilon\frac{n}{x^{n+1}}\\]

となり、分数についても先ほどの性質が成り立つことがわかります。

## 二重数を用いた自動微分
先ほどの定義により、少なくとも上で言及した関数とその合成である\\(f\\)に関しては、\\(\frac{\mathrm{d}f(x)}{\mathrm{d}x}\\) は \\(f(x + \epsilon)\\) の \\(\\epsilon\\) の係数を求めるだけで求めることができることがわかります。

これを用いて関数の微分を自動で行うコードを書いてみました。

```haskell
{-# LANGUAGE Rank2Types, ConstraintKinds #-}

module Dual where

data Dual a = !a :+: !a
  deriving (Show, Read, Eq, Ord)

infixl 6 :+:

instance Num a => Num (Dual a) where
  (a :+: b) + (a' :+: b') = (a + a') :+: (b + b')
  (a :+: b) - (a' :+: b') = (a - a') :+: (b - b')
  (a :+: b) * (a' :+: b') = (a * a') :+: (a * b' + a' * b)
  fromInteger n = fromInteger n :+: 0
  -- abs, signumは使わないので略💩

instance Real a => Real (Dual a) where
  toRational (a :+: _) = toRational a

instance Fractional a => Fractional (Dual a) where
  (a :+: b) / (c :+: d) = (a / c) :+: (b * c - a * d) / (c * c)
  fromRational a = fromRational a :+: 0

instance Floating a => Floating (Dual a) where
  pi = pi :+: 0
  exp (x :+: y) = (exp x :+: 0) * (1 :+: y)
  sin (x :+: y) = sin x :+: y * cos x
  cos (x :+: y) = cos x :+: (-y) * sin x
  log (x :+: y) = log x :+: (y / x)
  -- asin, acos, atan, sinh, cosh, ... などについては省略

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

ここで定義された関数`deriv`を用いると、`Real a, Floating a`を満たす任意の型`a`を取れる関数`f :: a -> a`に対して、その微分を求めることができます。

```haskell
*Main Lib> :l Dual.hs
*Dual> f x = 1 / (x^2)
*Dual> f' = deriv f   -- == -2/x^3
*Dual> f' 2           -- == -2/8 == -1/4
-0.25
*Dual> g x = 3 * x^2 + 2 * x + 1
*Dual> g' = deriv g   -- == 6 * x + 2
*Dual> g' 5           -- == 30 + 2 == 32
32.0
*Dual> g'' = deriv g' -- == 6 (定数関数)
*Dual> g'' 10
6.0
*Dual> g'' 12345
6.0
```
