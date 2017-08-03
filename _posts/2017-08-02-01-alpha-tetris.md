---
layout: post
title: 'テトリスの盤面の評価関数'
tags:
- Other
---

ふと思い立って、テトリスを自動で解くAIを書いてみました。

最初はαβ法でやってみようと思ったのですが、ブロック1つに対して置き場所の候補が3〜40通りくらいあり、探索空間がアホみたいに広くなってしまったため断念。とりあえず探索の深さを1だけにして、評価関数だけで攻めていく方針でやっています。

テトリスの場合はゲームとしてかなり単純なので、あまり深く先読みをしなくてもうまくやればうまくいきそうな気がします。具体的には、盤面の評価が

+ 積まれているブロックが少ない盤面
+ 積まれているブロックは多いが、消しやすい盤面
+ 積まれているブロックが多い上に、消しにくい盤面

の順番で高くなるようにしてやれば、深く探索しなくてもそこそこの性能は出せそうな気がします。

というわけで、上の評価値を算出するためのファクターとして、とりあえず以下のようなものを用意してみました。

1. 積まれているブロックの最大の高さ
2. 動かしているブロックの高さの平均値
3. ブロックをその位置に置くことで消せる列の数
4. 孤立した(四方をブロックで囲まれている)何もない空間の数

予想だと、(1), (2), (4) が小さく、 (3) が大きいほど優秀な盤面といえるでしょう。

というわけで、

```
-r1 * (1) - r2 * (2) + r3 * (3) - r4 * (4)
```

が最大となるような積み方を選ぶようなAIを組んでみました(r1, r2, r3, r4は適宜調整)。

その結果がこちら。

![/img/post/2017-08-02-tetris-ai.png](/img/post/2017-08-02-tetris-ai.png)

動画にするのが面倒だったので盤面の一部の画像ですが、これだけ見てもお世辞にもうまい手を選べているとは言えませんね……。

次は気が向いた時にでもこれらのパラメーターを遺伝的アルゴリズムか何かでいい感じに調整するようにするつもりです。