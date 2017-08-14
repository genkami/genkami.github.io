---
layout: post
title: AV女優判定機を高速化した
tags:
- Others
---

[ここ](http://dokodeglobal.com/av_classifier/)で公開しているAV女優判定機を半年ぶりくらいにアップデートしたので、細かい内容のメモでもしておきます。

今回の更新内容

+ ナイーブベイズの計算中に何度も出て来る値を定数として持っておくことで、計算の高速化
+ リクエストごとに分類器を作っていたのを、予め用意したものを使いまわすように変更
+ MeCabの辞書を[mecab-ipadic-neologd](https://github.com/neologd/mecab-ipadic-neologd)を使うようにし、語彙を強化
+ 分類器の保存方法を見直し、サイズの圧縮

それぞれのベンチマーク結果と、分類器のサイズが以下の通り。

古いほう:

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

上から順番に、文書からの分類器の構築、構築した分類器のタンプ、ダンプしたデータからの分類器のロード、分類器による文書の分類にかかる時間と、ダンプした分類器のデータのサイズを表しています。

新しいほう:

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

dumpとloadにはかなり時間がかかるようになってしまいましたが、サーバーの起動時に一回読み込むだけにしたため、それほど大きな問題にはならないはずです。

むしろ、classifyの速度が数倍になっていることに注目してください。実際にAV女優判定機が動いている間に使われるのはclassifyだけなので、ここの速度が重要になってきます。

実際、今回のアップデートで体感上の速度は倍程度になったはずです。

また、分類器のデータを半分程度に縮めることもできました。
