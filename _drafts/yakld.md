---
layout: post
title: 自作SchemeインタプリタでSICPをやるという夢
tags:
- YAKLD
- Scheme
---

この記事は、 [ミクシィグループ Advent Calendar 2018](https://qiita.com/advent-calendar/2018/mixi) の25日目の記事です。

突然ですが、みなさんは自作Scheme処理系でSICPをやりたくなったことはありますか？

僕はあります。なのでScheme処理系を書きました。以下がその処理系のソースコードになります:

[genkami/YAKLD: YAKLD: Yet Another JAKLD](https://github.com/genkami/YAKLD)

このYAKLDは、京都大学大学院情報学研究科にかつて存在していた [湯浅研究室](http://www.yuasa.kuis.kyoto-u.ac.jp/~yuasa/index_J.html) により開発された、 [JAKLD](http://www.yuasa.kuis.kyoto-u.ac.jp/~yuasa/jakld/index-j.html) というScheme処理系がベースになっています。このJAKLDを元に、ほぼすべてのコードをScalaで書き直し、ついでに機能もモリモリ追加してそれなりに使えるSchemeインタプリタにしてやろうじゃないかという目標のもとでYAKLDは開発されています。元のコードはほとんど残っていないので、そろそろ自作Scheme処理系と言っても問題ないかな？と思いこのようなタイトルにしました。

## なんでやることにしたか
僕が去年まで在籍していた京都大学工学部情報学科では、1回生(たしか)の講義であるプログラミング言語入門のような講義(正式名称は忘れた)でこのJAKLDという言語処理系が使用されていました。おそらく、どんな環境のPCでもとりあえず動かせる処理系を用意しておくことで、環境構築のハードルを下げるという意図によるものだったの思うのですが、いかんせんJAKLDはどマイナーな言語であるためググっても情報が出ない、そもそもドキュメントが無い等々、とにかくプログラミング初心者にとっては地獄のような状況(主観)となっておりました。

このような状況に対する反省もあったのかなかったのか、翌年度からは講義でJAKLDは利用されなくなり、代わりにJavaかなにかの割と普通の言語が使われるようになったようです。それはそれでいいことですが、僕たちが一年間(半年だったかも)を共にしたJAKLDがこのまま忘れ去られてしまうのもなんとなく悲しいので、「それなりに使えるJAKLD」のようなものをインターネット上に残しておきたいと思うようになったのが開発の動機です。

~~というのは建前で、JAKLD自体は3500行程度とそこまで大きくないので、さっと書き直してアドベントカレンダーのネタにしやすかったというのもあります。~~

## Scalaへの移行の戦略と、現在の開発状況
JAKLDはもともとJavaで書かれてるSchemeインタプリタでした。Schemeのすべての式は `java.lang.Object` にキャストされていました。個人的にはそのままだと非常に書きにくく、Scalaの `case class` を使ってスッキリ書けるようにしていきたかったので、以下のような手順でJavaで書かれたJAKLDをScalaで書かれたYAKLDに置き換えていきました(正確には置き換えている途中です)。

1. 大まかに全体をScalaに書き直す
  + この時点では、ただただJavaのコードの文法がScalaになっただけです。
  + このへんのコードが `src/main/scala/me/_7nobo/yakld/` 以下に書き散らかされています。
  + このコードは後で捨てられる前提なので、この時点ではテストは書かず、雑なSchemeのコードを実行して、実行結果がおかしくないかの確認程度しかしていません。
2. Scalaの `case class` を使ったSchemeの式、それに対する `eval` 等を用意した、小さなインタプリタを作る
  + このへんが `src/main/scala/me/_7nobo/yakld/core` 以下のコードになります
3. JAKLDの式を表すオブジェクトと、YAKLDの式を表すオブジェクトの相互変換を行えるようにし、JAKLDの世界とYAKLDの世界を相互に行き来しながらインタプリタを実行できるようにする
4. Scalaでがしがし機能追加をする
  + このへんからテストも書き始めました
5. 4と平行して、JAKLDの世界のコードを少しずつYAKLDの世界のコードに置き換えていく
6. YAKLDの世界のコードが全部消えたらめでたく移行完了

現状としては、4〜5の途中くらいです。JAKLDに比べて見た目上の機能の追加は今の所全くありませんが、言語のコア部分がJAKLDより小さくなったことで、JAKLDよりも機能追加などはしやすくなったかと思います。

## 今後の目標

YAKLDは現在、以下の目的をもって開発されています:
* JAKLDより初心者の学習に向いている言語
* 処理系のコア部分は小さく保ち、拡張性を高くできるようにする
* JAKLDのもともとのコンセプトである「組み込み用のLispドライバ」であることは捨てる
* R7RS-small準拠(R7RS-largeまで実装するかは時間と気力次第)

そのために、当面の作業としては以下のような実装を行っていくつもりです:
* まずはR7RS-small準拠を目指す
  + ちゃんとした継続を使えるようにする(JAKLDの継続は制約が多く、あまり使えない)
  + マクロを実装する
* 「学習用」であるための一つの指標として、YAKLDだけでSICPをやれるようにすることを目標にする
  + そのためには図形言語等のライブラリも実装する必要がある
* ドキュメントを整備する

趣味でやっているため完成がいつになるかはわかりませんが、暇な時に少しずつやっていきたいと思います。

## 実行方法

実行方法は以下の通りです。今の所基本的なFizz Buzzくらいは動くことが確認できています。

```scheme
$ git clone git@github.com:genkami/YAKLD.git
$ cd YAKLD
$ sbt run
>(define (fizz-buzz x)
  (if (>= 15 x)
    (let ((x-mod-15 (remainder x 15))
          (x-mod-5  (remainder x 5))
          (x-mod-3  (remainder x 3)))
      (cond
        ((= x-mod-15 0) (display "Fizz Buzz") (newline))
        ((= x-mod-3  0) (display "Fizz") (newline))
        ((= x-mod-5  0) (display "Buzz") (newline))
        (else           (display x) (newline)))
      (fizz-buzz (+ x 1)))))
fizz-buzz

>(fizz-buzz 1)
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
Fizz Buzz
()
```

## 最後に
ほしい物リストはこちら :point_right: [http://amzn.asia/dnREJ0l](http://amzn.asia/dnREJ0l)

みなさんクリスマスプレゼントお待ちしております :christmas_tree: :santa:
