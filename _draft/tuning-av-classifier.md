---
layout: post
title: 
tags:
- 
---

古いほう
そもそも元の目的がナイーブベイズの分類器を作ることじゃなく、適当にアルゴリズムを実装していたら最終的にできてしまったみたいな感じなのでごちゃごちゃしてるし、無駄が多い。同じ計算何回もやってる部分とかもある気がする。

新しいほう
コードを色々とすっきりさせる
分類器の出力フォーマットの見直し。サイズはかなり小さくできるようになったはず。

一から作り直したのですっきり

古いほう

```
$ gosh bin/bench-all.scm
build: #<time-result 1 times/ 63.912 real/124.000 user/  1.630 sys>
dump: #<time-result 10 times/  7.813 real/  9.560 user/  0.070 sys>
load: #<time-result 10 times/  1.987 real/  2.140 user/  0.020 sys>
classify: #<time-result 10 times/  1.219 real/  0.970 user/  0.170 sys>
done
$ ls -lh resources/classifier.data # ダンプした分類器のデータ
-rw-r--r--  1 admin  staff   3.0M  8  6 01:29 resources/classifier.data
```

新しいほう

```
$ gosh bin/bench-all.scm
build: #<time-result 1 times/ 36.651 real/ 37.000 user/  1.060 sys>
dump: #<time-result 10 times/ 14.512 real/ 17.690 user/  0.160 sys>
load: #<time-result 10 times/ 25.524 real/ 30.500 user/  0.220 sys>
classify: #<time-result 10 times/  0.291 real/  0.130 user/  0.080 sys>
done
$ ls -lh resources/classifier.data
-rw-r--r--  1 admin  staff   1.7M  8  6 01:41 resources/classifier.data
```

dumpとloadには倍くらい時間がかかるようになってしまったが、buildの速度は倍、classifyに至っては数倍の速度が出ている。

正直loadが遅いのは一度起動してしまえば対した問題にならないので、classifyが速いことが一番重要

