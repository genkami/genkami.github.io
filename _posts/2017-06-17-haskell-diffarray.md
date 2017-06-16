---
layout: post
title: 破壊的変更を隠蔽するDiffArray
tags:
- Haskell
---

Haskellの`Array`は要素の更新を行うたびに配列全体のコピーが発生するので、頻繁な書き換えには向いていません。

``` haskell
-- ArrayTest.hs
import Control.Monad
import Data.Array.IArray
import System.IO.Unsafe
import System.Random

randomVals :: Random a => Int -> (a, a) -> IO [a]
randomVals n (min, max) = unsafeInterleaveIO $ replicateM n $ randomRIO (min, max)

-- 先頭の要素から順に vals の値で書き換える
writeVals :: Array Int e -> [e] -> Array Int e
writeVals arr vals = write 0 arr vals
  where
    write n arr [] = arr
    write n arr (e : es) = write (n + 1) (arr // [(n, e)]) es

main :: IO ()
main = do
  let n = 10000
  nums <- randomVals n (0, 100) :: IO [Int]
  let arr = array (0, n - 1) []
      arr' = writeVals arr nums
  print $ foldl (+) 0 arr'
```

例えば上のコードはたった1万回のループですが、1回のループごとに配列のコピーが発生するため、実行時間は非常に遅くなります。

``` sh
$ stack exec ghc ArrayTest.hs
[1 of 1] Compiling Main             ( ArrayTest.hs, ArrayTest.o )
Linking ArrayTest ...
$ time ./ArrayTest
499989

real	0m0.838s
user	0m0.821s
sys	0m0.011s
```

このようなコピーをなくすためには、`STArray`や`IOArray`などの`MArray`を使うという手があります。

``` haskell
-- STArrayTest.hs
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unsafe
import System.IO.Unsafe
import System.Random

randomVals :: Random a => Int -> (a, a) -> IO [a]
randomVals n (min, max) = unsafeInterleaveIO $ replicateM n $ randomRIO (min, max)

writeVals :: Array Int e -> [e] -> Array Int e
writeVals arr vals = runSTArray $ do
  arr' <- unsafeThaw arr
  forM_ (zip [0..] vals) $ \(i, v) -> do
    writeArray arr' i v
  return arr'

main :: IO ()
main = do
  let n = 10000
  nums <- randomVals n (0, 100) :: IO [Int]
  let arr = array (0, n - 1) []
      arr' = writeVals arr nums
  print $ foldl (+) 0 arr'
```

先ほどとほとんど同じコードですが、`STArray`と`unsafeThaw`を使うことでコピーが起きないようにしています。

これを実行してみると、先程の`Array`のバージョンより数十倍早くなっていることがわかります。

``` sh
$ stack exec ghc STArrayTest.hs
[1 of 1] Compiling Main             ( STArrayTest.hs, STArrayTest.o )
Linking STArrayTest ...
$ time ./STArrayTest
506073

real	0m0.039s
user	0m0.016s
sys	0m0.008s
```

しかし、`STArray`や`IOArray`を多用してしまうと記法がどうしても手続き的になり、Haskellらしくかけないという問題があります。

これを解決してくれるのが`DiffArray`です。

[diffarray: DiffArray](https://hackage.haskell.org/package/diffarray)

この`DiffArray`は内部に`IOArray`を持ち、`//`演算子で要素の変更を行う際に、実際に`unsafePerformIO`で破壊的変更をしてしまいます。

``` haskell
Prelude> import Data.Array.Diff
Prelude Data.Array.Diff> let arr = listArray (0, 9) [i * 2 | i <- [0..9]] :: DiffArray Int Int
Prelude Data.Array.Diff> arr
array (0,9) [(0,0),(1,2),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16),(9,18)]
Prelude Data.Array.Diff> let arr1 = arr // [(1, 5)]
Prelude Data.Array.Diff> arr1
array (0,9) [(0,0),(1,5),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16),(9,18)]
```

しかし、単に破壊的変更を行ってしまうのであれば、もとの`arr`の値も`arr1`に等しくなってしまうはずです。

`DiffArray`では少し特殊な方法でこれを回避しており、配列の要素の破壊的変更を行って新しい配列を作る際、ついでに古い配列の指し示している値も変更し、新しい配列と変更された要素の情報の組として持たせます。そのため、古い配列にアクセスするのにかかる時間は若干遅くなってしまいますが、頻繁に更新が発生するような状況で変更前の配列にアクセスすることは実用上まずないので、この点はあまり問題になりません。

``` haskell
Prelude Data.Array.Diff> arr
array (0,9) [(0,0),(1,2),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16),(9,18)]
Prelude Data.Array.Diff> arr1
array (0,9) [(0,0),(1,5),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16),(9,18)]
Prelude Data.Array.Diff> arr ! 1 == arr1 ! 1
False
```

実際に`arr`は変更前の値が保たれていることがわかるかと思います。

では実際に`DiffArray`を使って先ほどと同じコードを書いてみましょう。


``` haskell
import Control.Monad
import Data.Array.Diff
import Data.Array.IArray
import System.IO.Unsafe
import System.Random

randomVals :: Random a => Int -> (a, a) -> IO [a]
randomVals n (min, max) = unsafeInterleaveIO $ replicateM n $ randomRIO (min, max)

writeVals :: IArray a e => a Int e -> [e] -> a Int e
writeVals arr vals = write 0 arr vals
  where
    write n arr [] = arr
    write n arr (e : es) = write (n + 1) (arr // [(n, e)]) es

main :: IO ()
main = do
  let n = 10000
  nums <- randomVals n (0, 100) :: IO [Int]
  let arr = array (0, n - 1) [] :: DiffArray Int Int
      arr' = writeVals arr nums
  print $ sum $ elems arr' -- DiffArray は Foldable のインスタンスではない
```

実行してみると、さすがに`STArray`には及ばないものの、それでもかなり高速に動いていることがわかります。


``` sh
$ stack exec ghc DiffArrayTest.hs
[1 of 1] Compiling Main             ( DiffArrayTest.hs, DiffArrayTest.o )
Linking DiffArrayTest ...
$ time ./DiffArrayTest
502155

real	0m0.041s
user	0m0.030s
sys	0m0.007s
```