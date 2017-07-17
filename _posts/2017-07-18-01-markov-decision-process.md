---
layout: post
title: マルコフ決定問題を解く
tags:
- Algorithm
- Haskell
use_mathjax: true
---

例えば、A社とB社のどちらの株を買うかで迷っているとします。

A社の株は安定してそこそこ儲かりそう。B社の株は損をしそうですが、もしかしたらドカンと当たるかもしれない。

このような状況を(簡略化して)図にしてみると以下のようになります。

![/img/post/2017-07-18-markov-decision.png](/img/post/2017-07-18-markov-decision.png)

このように、ある動作を行った時に確率的に状態が遷移していくような過程をマルコフ決定過程といい、この過程で利益を最大化するような方策を求める問題をマルコフ決定問題といいます。

この問題を定式化すると、以下のようになります。

+ \\( S = \\{ s_1, s_2, ..., s_N \\} \\): 状態の有限集合
+ \\( U = \\{ u_1, u_2, ..., u_M \\} \\): 行動の有限集合
+ \\( p_{ijk} \\): 状態 \\(s_i\\) で行動 \\(u_k\\) を取った時に状態 \\(s_j\\) に遷移する確率
+ \\( c_{ijk} \\): 状態 \\(s_i\\) で行動 \\(u_k\\) を取って状態 \\(s_j\\) に遷移した時に直接得られる利益
+ \\( c_{ik} \\): 状態 \\(s_i\\) で行動 \\(u_k\\) を取った時に直接得られる利益の期待値 \\( (= \sum_{j}p_{ijk}c_{ijk}) \\)

例えば、上の図の例だと

+ \\( S = \\{ s_0, s_1, s_2 \\}, s_1 = s_儲, s_2 = s_損 \\)
+ \\( U = \\{ u_0, u_1, u_2, u_3, u_4 \\} \\)
+ \\( u_0 = \\) A社の株を買う
+ \\( u_1 = \\) B社の株を買う
+ \\( u_2 = \\) 株を買わない
+ \\( u_3 = \\) 株を売らない
+ \\( u_4 = \\) 株を売る
+ \\( p_{010} = 1 \\)
+ \\( c_{010} = -100000 \\)
+ ...

といったようになります。

このとき、状態 \\(s_i\\) から無限に時間が経った時に得られる利益の期待値を \\(h^*(i)\\) をおくと、以下の式が成り立つと考えることができます。

\\( h^\*(i) = \max_{k} \\{ c_{ik} + \gamma \sum_{j}p_{ijk} h^\*(j) \\} \\)

ここで、 \\(\gamma~(0 \leq \gamma \leq 1)\\) は割引率といい、将来の利益を割り引いて考えるための定数になります。

実際、ほとんどの人は「今1万円くれたら10万円あげるよ」「今1万円上げたら5年後に10万円にして返すよ」の2択なら、間違いなく前者を選ぶはずです。そういった考えを反映しているのが \\(\gamma\\) というわけです。

なお、\\( \gamma = 1 \\) のときは \\(h^*(i)\\) が無限大に発散する可能性があるので、注意が必要です。

この \\(h^*(i)\\) を求めるための漸化式を考えてみましょう。

\\(Q_t(i, k)\\)を、「状態 \\(s_i\\) で行動 \\(u_k\\) を取り、その後 \\(t\\) 回適切な行動を選んで状態遷移した場合に得られる利益の期待値の最大値」とします。このとき、

\\[ Q_0(i, k) = c_{ik} \\]
\\[ Q_{t+1}(i, k) = c_{ik} + \gamma \sum_j p_{ijk} \max_{k'}\\{ Q_t(j, k') \\} \\]

が成り立つことがわかります。

この漸化式は、動的計画法を使うことによって簡単に計算することができます。

```haskell
-- MDP.hs
import Control.Monad
import Data.Array (Array, (!))
import qualified Data.Array as Array

type Action = Int -- u_0, ..., u_5
actionMin = 0
actionMax = 4

type State = Int -- s_0, ..., s_2
stateMin = 0
stateMax = 2

p :: Array (State, State, Action) Double
p = Array.array ((stateMin, stateMin, actionMin), (stateMax, stateMax, actionMax))
  [ ((i, j, k), case (i, j, k) of
        (0, 1, 0) -> 1   -- s0 A社の株を買う
        (0, 2, 1) -> 1   -- s0 B社の株を買う
        (0, 0, 2) -> 1   -- s0 株を買わない
        (1, 1, 3) -> 0.8 -- s1 株を売らない
        (1, 2, 3) -> 0.2 -- s1 株を売らない
        (1, 0, 4) -> 1   -- s1 株を売る
        (2, 1, 3) -> 0.1 -- s2 株を売らない
        (2, 2, 3) -> 0.9 -- s2 株を売らない
        (2, 2, 4) -> 1   -- s2 株を売る
        (_, _, _) -> 0)
  | i <- [stateMin..stateMax]
  , j <- [stateMin..stateMax]
  , k <- [actionMin..actionMax] ]

c :: Array (State, State, Action) Double
c = Array.array ((stateMin, stateMin, actionMin), (stateMax, stateMax, actionMax))
  [ ((i, j, k), case (i, j, k) of
        (0, 1, 0) -> -100000 -- s0 A社の株を買う
        (0, 2, 1) ->  -70000 -- s0 B社の株を買う
        (0, 0, 2) ->       0 -- s0 株を買わない
        (1, 1, 3) ->   20000 -- s1 株を売らない
        (1, 2, 3) ->  -50000 -- s1 株を売らない
        (1, 0, 4) ->   80000   -- s1 株を売る
        (2, 1, 3) ->  100000 -- s2 株を売らない
        (2, 2, 3) ->  -10000 -- s2 株を売らない
        (2, 2, 4) ->   30000   -- s2 株を売る
        (_, _, _) ->       0)
  | i <- [stateMin..stateMax]
  , j <- [stateMin..stateMax]
  , k <- [actionMin..actionMax] ]

c' :: Array (State, Action) Double
c' = Array.array ((stateMin, actionMin), (stateMax, actionMax))
  [ ((i, k), sum [ (c ! (i, j, k)) * (p ! (i, j, k)) | j <- [stateMin..stateMax] ])
  | i <- [stateMin..stateMax]
  , k <- [actionMin..actionMax] ]

gamma = 0.7
dpMax = 100

-- dp ! (t, i, k) == Q_t(i, k)
dp :: Array (Int, State, Action) Double
dp = Array.array ((0, stateMin, actionMin), (dpMax, stateMax, actionMax))
  [ ((t, i, k), case t of
        0 -> c' ! (i, k)
        _ -> c' ! (i, k) + gamma * sum [ p ! (i, j, k) *
                                         maximum [ dp ! (t - 1, j, k')
                                                 | k' <- [actionMin..actionMax] ]
                                       | j <- [stateMin..stateMax] ])
  | t <- [0..dpMax]
  , i <- [stateMin..stateMax]
  , k <- [actionMin..actionMax] ]

main = do
  putStrLn $ "A社の株を買う場合: " ++ show (dp ! (dpMax, 0, 0))
  putStrLn $ "B社の株を買う場合: " ++ show (dp ! (dpMax, 0, 1))
  putStrLn $ "株を買わない場合: " ++ show (dp ! (dpMax, 0, 2))
```

実行すると、初期状態から「A社の株を買う」「B社の株を買う」「株を買わない」のそれぞれの行動を選んだ場合の、\\(\gamma = 0.7\\) での利益の期待値の最大値を表示してくれます。

```
$ stack exec ghc MDP.hs
[1 of 1] Compiling Main             ( MDP.hs, MDP.o )
Linking MDP ...
$ ./MDP
A社の株を買う場合: -44000.0
B社の株を買う場合: -2.9103830456733704e-11
株を買わない場合: 0.0
```

どちらの株を買っても損することがわかりました。株はクソ。
