---
layout: post
title: Makefile"のみ"でFizz Buzz
tags:
- Linux
---

Fizz Buzzを変態的な環境で書こうという試みは世界中で行われており、MakefileによるFizz Buzzも例外ではありません。適当にググってみたところ、既存のMakefileによるFizz Buzzを謳ったコードは以下の3箇所で見つかりました。

1. [FizzBuzz - Rosetta Code](https://rosettacode.org/wiki/FizzBuzz#make)
2. [時代遅れひとりFizzBuzz祭り　make編（gmakeでもpmakeでも書いてみせらあ。でもnmakeだけは勘弁な！） - 新・日々録 by TRASH BOX@Eel](http://d.hatena.ne.jp/eel3/20110924/1316791928)
3. [fizzbuzz in GNU make](https://gist.github.com/g-k/4381991)

しかし、これらのコードにはいずれも欠点があり、純粋にMakefileのみでFizz Buzzを書いたとは言えません。

実際、上の1.のコードは、`jot`や`expr`などのコマンドを多用しており、実質シェルスクリプトと言っても過言ではありません。Fizz Buzzを書く上で肝心の部分である剰余の計算などをすべて外部のコマンドに任せてしまっているのは、MakefileのみではFizz Buzzは書けないと認めてしまったようなものです。

また、2.のコードは`seq`コマンドを使って3の倍数の列や5の倍数の列などを生成しており、これを元に倍数の判定を行っています。1.のコードよりはMakefileらしいと言えるかもしれませんが、それでも改良の余地があると言えます。

最後に挙げた3.のコードはかなりいい線を言っていますが、倍数判定などの部分を[GMSL](https://gmsl.sourceforge.net/)という外部ライブラリに頼り切っています。GMSLはMakefileのみで書かれたライブラリなので、このコード自体はMakefileによるFizz Buzzを名乗るのにふさわしいかもしれません。しかし、たかがFizz Buzzごときにこんな大仰なライブラリを使ってしまうのは筋が悪いのではないでしょうか。もっとシンプルにFizz Buzzを書く方法は存在するはずです。

というわけで、今回は以下のルールを課すことにより、本当の意味で純粋にMakefileのみで書かれたFizz Buzzを実装しました。

+ シェルに渡せるコマンドは`echo`のみ
+ 外部ライブラリは一切使用しない

また、今回使用する`make`はGNU Make 3.81(MacBookに気がついたらインストールされてたやつ)に限定します。

## MakefileによるFizz Buzzの何が難しいのか
前述した通り、MakefileでFizz Buzzを書こうという試みはこれまでに何度もなされているであろうにもかかわらず、その中で純粋にMakefileのみで書かれたFizz Buzzと言えるものはありません。既存のコードは、Fizz Buzzの重要な部分の一つである倍数判定を外部のコマンドやライブラリに頼り切っています。このことから、MakefileでFizz Buzzを書く上で一番の壁となるのが数値の計算であると考えることは妥当でしょう。

事実として、Makefileには数値を扱うための機能や関数はほとんどありません。唯一数値に関係する関数と言えるのは、スペース区切りの単語列に対して単語の数を数える`words`関数のみです。

その代わり、Makefileには豊富な文字列処理関数があり、以下のような操作は簡単に行うことができます。

+ ある文字列が空文字列であるかどうかの判定
+ ある文字列が他の文字列の部分文字列になっているかどうかの判定
+ ある文字列に対して特定の接頭辞/接尾辞を付加
+ ある文字列に対して特定の接頭辞/接尾辞を削除

MakefileでFizz Buzzを書くためのキモとなるのは、これらの操作をうまく組み合わせていかに数値の加減乗除を行うかという所にありそうです。

## 実装
今回の実装では、数値`n`を「文字`x`の`n`回の繰り返し」で表します。なお、GMSLでは数値`n`をスペース区切りの`n`個の単語列で表しているようなのですが、前者の実装のほうが後述するようにFizz Buzzを書くために必要な処理が簡潔に書けるため、こちらを採用します。

また、以下では文字`x`の`n`回の繰り返しを`"x"^n`というように書きます。これを用いて、Fizz Buzzを書くために必要な計算を順に実装していきます。

### 0であるかどうかの判定
文字列`s`が`"x"^0`、すなわち空文字列であるかどうかの判定を行えばよいということがわかります。幸い、Makefileでは空文字列はFalse的な扱いになるため、例えば

```make
$(if s,(sが0以上を表す文字列のとき),(sが0を表す文字列のとき))
```

というように条件分岐を行うことで、0であるかどうかの判定を行うことができるようになります。

### 大小比較
文字列`s = "x"^n`と`t = "x"^m`に対して、`n <= m`であるかどうかは、`s`が`t`の部分文字列であるかどうかで判断することができます。具体的な方法としては、

```make
$(findstring s,t)
```

が空でない文字列を返す場合は`n <= m`、空文字列を返す場合は`n > m`と判断できます。

### 定数を引く
文字列`s = "x"^n`について、任意の`t = "x"^m`を`s`から引き算するのは少々面倒ですが、`m`が固定されている場合の引き算は簡単に行うことができます。`s`から接尾辞としての`"x"^m`を消去すれば、残った文字列は`"x"^(n-m)`となります。このような処理は、`patsubst`関数を用いて行うことができます。以下は`s`から末尾の`x`を5個取り除く例です。

```make
$(patsubst %xxxxx,%,s)
```

### 10進数への変換
先ほど「唯一数値に関係する関数と言えるのは、スペース区切りの単語列に対して単語の数を数える`words`関数のみ」と書きましたが、この`words`関数を用います。`s = "x"^n`に対して、そこに現れるすべての`"x"`を`"x "`に変換してしまえば、変換後の文字列は`n`個の単語`"x"`からなるスペース区切りの単語列となります。これに対して`words`関数を適用すれば、その結果は数値`n`となります。

```make
$(words $(subst x,x ,s))
```

以上の方法を組み合わせることによって、Fizz Buzzに必要な処理を簡単に書くことができます。例えば、`n`が3の倍数であれば`"Fizz"`を、そうでない場合は空文字列を返す関数`FIZZ`は以下のような擬似言語で書くことができます。

```
FIZZ(n) =
  if n >= 3 then FIZZ(n - 3)
  else if n != 0 then ""
  else "Fizz"
```

ここで使われている処理は「3以上かどうかの比較」「0であるかどうかの比較」「`n`から定数3を引く引き算」の3種類であり、これらはすべて上述した方法で実行することができます。

`n`が5の倍数であれば`"Buzz"`を返す関数に関しても、同様の方法で実装することができます。

最後に必要になってくるのが1〜100までのループですが、これは単に

```make
nについてのターゲット: (n-1)についてのターゲット
	@echo (nに対するFizz Buzzの結果)
```

というようにルールを作ってしまえば、後は依存関係を辿って1から順にFizz Buzzの結果が出力されるようになります。

上記の結果をすべてまとめたものが以下のソースコードになります。

## ソースコード

```make
MAX := xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

all: fb-$(MAX)x

ITER_NUMS = $(if $1,$(call ITER_NUMS,$(patsubst %x,%,$1)) $1,)

ALL_NUMS := $(call ITER_NUMS,$(MAX))

FIZZ = $(if $(findstring xxx,$1),\
	$(call FIZZ,$(patsubst %xxx,%,$1)),\
	$(if $1,,Fizz))

BUZZ = $(if $(findstring xxxxx,$1),\
	$(call BUZZ,$(patsubst %xxxxx,%,$1)),\
	$(if $1,,Buzz))

FB_STEP = $(or $(strip $(call FIZZ,$1) $(call BUZZ,$1)),$(words $(subst x,x ,$1)))

define FB_RULE
fb-$(1)x: fb-$(1)
	@echo $(call FB_STEP,$1)

endef

$(eval $(foreach n,$(ALL_NUMS),$(call FB_RULE,$n)))

fb-x:
```

全体で27行です。

一番上に挙げた既存のMakefileによるFizz Buzzのコードのうち、1.のコードは28行、2.のコードのうち、GNU Make版で新しいほうのものは21行、3.のコードは33行なので、外部コマンドやライブラリを使用しなかったからといって、極端にFizz Buzzを書くために必要な行数が増えてしまうということは無さそうです。

## 実行結果

```sh
$ make
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
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
Fizz Buzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
Fizz Buzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
Fizz Buzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
Fizz Buzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
Fizz Buzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz
```

念のため、この結果をPythonで書かれた以下のようなFizz Buzzのコードの出力と比較してみました。

```python
for i in range(1, 101):
    if i % 15 == 0:
        print('Fizz Buzz')
    elif i % 3 == 0:
        print('Fizz')
    elif i % 5 == 0:
        print('Buzz')
    else:
        print(i)
```

```sh
$ make > fizzbuzz-makefile
$ python fizzbuzz.py > fizzbuzz-python
$ diff fizzbuzz-makefile fizzbuzz-python
$
```

両方の出力が一致したため、出力が間違っているということは無さそうです。
