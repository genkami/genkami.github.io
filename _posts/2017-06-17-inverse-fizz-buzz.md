---
layout: post
title: 逆FizzBuzzをHaskellで解いてみた
tags:
- Haskell
---

逆FizzBuzz問題というのがあるらしいです。

[逆FizzBuzz問題 (Inverse FizzBuzz) - 猫とC#について書くmatarilloの雑記](http://d.hatena.ne.jp/matarillo/20120515/p1)

簡単に言うとFizzBuzzの逆関数を求める問題ですね。

Haskellで解いてみました。

``` haskell
-- FizzBuzzInv.hs
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (isPrefixOf, sortBy)

data FizzBuzz = Fizz | Buzz | FizzBuzz deriving (Eq, Show)

fizzBuzz :: Int -> Maybe FizzBuzz
fizzBuzz n
  | n `mod` 15 == 0 = Just FizzBuzz
  | n `mod`  3 == 0 = Just Fizz
  | n `mod`  5 == 0 = Just Buzz
  | otherwise       = Nothing

fizzBuzzFrom :: Int -> [(Int, FizzBuzz)]
fizzBuzzFrom n = catMaybes $ fbs n
  where
    fbs n = fmap (cons n) (fizzBuzz n) : fbs (n + 1)
    cons a b = (a, b)

findFizzBuzzSeq :: [FizzBuzz] -> [[(Int, FizzBuzz)]]
findFizzBuzzSeq fbs = do
  let len = length fbs
  i <- [3, 5, 6, 9, 10, 12, 15]
  let fbs' = fizzBuzzFrom i
  if fbs `isPrefixOf` (map snd fbs')
    then return $ take len $ fbs'
    else []

invFizzBuzz :: [FizzBuzz] -> Maybe (Int, Int)
invFizzBuzz fbs = case findFizzBuzzSeq fbs of
  [] -> Nothing
  xs -> let lengths = map (\fbs -> (fst $ head fbs, fst $ last fbs)) xs
        in Just $ head $ sortBy (compare `on` (\(x, y) -> y - x)) lengths

test :: [FizzBuzz] -> IO ()
test fbs = do
  putStr $ show fbs
  putStr " -> "
  case invFizzBuzz fbs of
    Just (from, to) -> print [from..to]
    Nothing -> putStrLn "X"

main :: IO ()
main = do
  test [Fizz]
  test [Buzz]
  test [Fizz, Fizz]
  test [Fizz, Buzz]
  test [Buzz, Fizz]
  test [Buzz, Buzz]
  test [Fizz, Buzz, Fizz]
  test [Fizz, Fizz, Buzz]
  test [Buzz, Fizz, Buzz]
  test [Buzz, Fizz, Fizz]
```

実行結果:

```
$ stack exec runghc FizzBuzzInv.hs
[Fizz] -> [3]
[Buzz] -> [5]
[Fizz,Fizz] -> [6,7,8,9]
[Fizz,Buzz] -> [9,10]
[Buzz,Fizz] -> [5,6]
[Buzz,Buzz] -> X
[Fizz,Buzz,Fizz] -> [3,4,5,6]
[Fizz,Fizz,Buzz] -> [6,7,8,9,10]
[Buzz,Fizz,Buzz] -> X
[Buzz,Fizz,Fizz] -> [5,6,7,8,9]
```

FizzBuzzほどシンプルな問題ではないので一瞬考えますが、15を周期としてFizzBuzzの列のパターンが繰り返されることにさえ気づけば簡単に書けます。
