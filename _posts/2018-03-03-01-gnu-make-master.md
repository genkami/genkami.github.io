---
layout: post
title: GNU Make 基礎文法最速マスター
tags:
- Linux
---

[これ]({% post_url 2018-03-02-01-makefile-fizzbuzz %})を書いていたらMakefileに関する知識がついてしまったので、ここにまとめておきます。

## 基本的な構文

makefileの基本的な構文は以下のようなものである:

```make
target1 target2 ... targetN: dep1 dep2 ... depM
	cmd1
	cmd2
	...
	cmdK
```

`target1`, ..., `targetN`をそれぞれ**ターゲット**という。ターゲットとは、基本的にはファイル名を表す。例外として、ファイル名ではないターゲットを作ることもできる(後述)。

`dep1`, ..., `depM`をそれぞれ**前提条件**という。前提条件も基本的にはファイル名を表す。これらは、ターゲットを生成するために必要となるファイル(等)の一覧である。

`cmd1`, ..., `cmdK`からなる部分を**レシピ**という。レシピはシェルに渡されるコマンドからなり、これらのコマンドを順に実行することでターゲットを生成することができることを表す。コマンドの前にあるのはタブ文字でなければならない。

このような、ターゲットを生成する方法の定義のことを、**ルール**という。makefileの基本的な構造は、このようにして定義されるルールの繰り返しである。

この状態で、`Makefile`のあるディレクトリ上で`make target1`を実行すると、`dep1`, ..., `depM`に関連するルールが実行された上で、`cmd1`, ..., `cmdK`が実行され、ファイル`target1`が生成される。
`target2`, ..., `targetN`についても同様。


## 実行方法

```make
out-file: file-1 file-2
	cat file-1 > out-file
	cat file-2 >> out-file

file-1:
	echo 'hogehogehoge' > file-1

file-2:
	echo 'fugafugafuga' > file-2
```

このような`Makefile`がある状態で`make out-file`というコマンドを実行すると、ターゲット`out-file`と、その前提条件に関するルールが順に実行される。

```sh
$ ls
Makefile
$ make out-file
echo 'hogehogehoge' > file-1
echo 'fugafugafuga' > file-2
cat file-1 > out-file
cat file-2 >> out-file
$ cat out-file
hogehogehoge
fugafugafuga
```

なお、makeは賢いので、ターゲットと前提条件にあるファイルの更新時刻を比較し、必要な場合にのみルールの実行を行う。例えば、先ほどの環境でもう一度`make out-file`を実行した場合、以下のようなメッセージが表示され、ルールは実行されない。

```sh
$ make out-file
make: `out-file' is up to date.
```

この状態でターゲットや前提条件にあるファイルを削除したり、前提条件にあるファイルを更新したりした場合は、再度ルールが実行される。

```sh
$ echo foobarfoobar >> file-2
$ make out-file
cat file-1 > out-file
cat file-2 >> out-file
$ cat out-file
hogehogehoge
fugafugafuga
foobarfoobar
```

なお、引数なしで`make`を実行した場合、`Makefile`に書かれている一番最初のルールが実行される。

```sh
$ make   # この場合は make out-file と同じ意味
make: `out-file' is up to date.
```

## ターゲット
### Phony Target
特定のファイルを生成したいわけではないが、何らかのコマンドを実行したい場合がある。
そのような場合は、**phony target**(偽のターゲットという意味) を用いる。あるターゲット`target`がphony targetであることを指定するには、`.PHONY: target`と書く。

```make
.PHONY: clean
clean:
	rm *.o
```

このとき、`clean`というファイルが実際に存在するかどうかにかかわらず、`make clean`と打てばルール`clean`が実行される。

## 変数

変数は、`var = value`もしくは`var := value`という形式で定義する。これらの違いについては後述。また、変数展開を行うときは`$var`もしくは`$(var)`のように書く。

```make
name = John Doe

.PHONY: greet
greet:
	echo Hello, $(name).
```

```sh
$ make greet
echo Hello, John Doe.
Hello, John Doe.
```

### 変数の味
変数には2つの種類(これを**味**という)がある。`var = value`で定義される変数は**再帰展開変数**といい、`var := value`で定義される変数は**単純展開変数**という。

単純展開変数は、シェルスクリプトの変数に近い挙動をする。代入の右辺に変数や後述する関数呼び出しなどがあった場合、単純展開変数ではこれらを代入のタイミングで展開する。例えば、

```make
fruit := orange
msg := I like $(fruit)

.PHONY: greet
greet:
	echo $(msg)
```

これは以下と全く同様である。


```make
fruit := orange
msg := I like orange

.PHONY: greet
greet:
	echo $(msg)
```

一方、再帰展開変数の場合は、実際に変数が展開されるまで、右辺の変数や関数呼び出し(後述)の展開を行わない。従って、後で右辺で使われている変数の値が書き換わった場合、それを使用している再帰展開変数の値も書き換わることになる。例えば、以下のmakefileを用いて`make greet`を行うと、`I like apple`が表示される。

```make
fruit = orange
msg = I like $(fruit)

fruit = apple

.PHONY: greet
greet:
	echo $(msg)
```

また、`+=`演算子を用いることによって変数の末尾に文字列を追加することができる。この際、変数の味は変わらない。

### 複数行からなる変数
展開した結果が複数行になるような変数は、以下のように`define`を用いて定義する:

```make
define CMDS
echo hello
echo goodbye
endef
```

`define`を用いて定義された変数は再帰展開変数となる。

なお、GNU Make 3.82以上では、`define VAR`の後に代入演算子`=`, `:=`を置くことが許される。この場合、変数`VAR`の味は置かれた演算子に対応するものとなる(未確認)。

### 特殊変数
いくつかの変数は自動で定義され、特殊な意味を持つ。以下はその一例である:

+ `$@`: ターゲット名
+ `$<`: 前提条件のうち、最初のもの
+ `$^`: 前提条件すべて

すべての特殊変数の一覧は、[公式のマニュアル](https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html)を参照。


## ルール
### 複数のルールを定義する
同一のターゲットに対して、複数のルールを定義してよい。

1つのターゲットに対しコマンドが書かれたルールが2つ以上あった場合は、それらの前提条件はすべて連結され、一番最後に定義されたルールのコマンドのみが実行される。


例えば、

```make
file1 file2: dep1 dep2

file1: dep3
	cat dep1 dep2 dep3 > file1

file2: dep4
	cat dep1 dep2 dep4 > file2
```

このmakefileは以下と等価である:


```make
file1: dep1 dep2 dep3
	cat dep1 dep2 dep3 > file1

file2: dep1 dep2 dep4
	cat dep1 dep2 dep4 > file2
```

### デフォルトのルールを定義する
特定のパターンにマッチするターゲットに対して、デフォルトのルールを定義することができる。例えば、以下で定義されている先頭のルールは、`*.o`ファイルを`*.c`から生成するルールである:

```make
%.o: %.c
	gcc -o $@ $<

hello.o: hello.h

goodbye.o: goodbye.h
```

`hello.o`, `goodbye.o`には明示的にコマンドが与えられていないため、デフォルトのルールに従って`gcc -o hello.o hello.c`というようにコマンドが実行される。

ここで定義したデフォルトのルールに含まれるような`%`を含む文字列は**パターン**と呼ばれ、`%`の部分に任意の1文字以上の文字列がマッチする。また、パターンを用いて定義されたルールを**パターンルール**という。

## レシピ
### シェルの変更
コマンドは各行ごとにシェルに送られる。

使用されるシェルは変数`$(SHELL)`で指定できる(デフォルトはsh)。例えばfishを使いたい場合は、以下のようにすればよい。

```make
SHELL := fish

.PHONY: hoge
hoge:
	if true; echo ok; else; echo error; end
```

### 実行するコマンドを出力しない
コマンドの先頭に`@`をつけると、実行するコマンド自体は表示せずに、シェルに`@`以下の文字列が送られる。例えば、以下のような`Makefile`を用意して`make hello`を実行すると、実行するコマンドである`echo hello, world`が表示され、その後に実行結果である`hello, world`が表示される。

```make
.PHONY: hello
hello:
	echo hello, world
```

```sh
$ make hello
echo hello, world
hello, world
```

一方、コマンドの先頭に`@`を付けた場合、コマンド自体は表示されない。

```make
.PHONY: hello
hello:
	@echo hello, world
```

```sh
$ make hello
hello, world
```

### コマンドのエラーを無視する
通常、makeは実行しているコマンドのうち一つがエラー(exit codeが0以外)となった場合、その時点で残りのコマンドの実行を中断する。

```make
.PHONY: fail
fail:
	false
	echo "OK!"
```

```sh
$ make fail
false
make: *** [fail] Error 1
```

特定のコマンドの返すエラーを無視したい場合は、そのコマンドの頭に`-`を付ける。その場合、対象のコマンドがエラーとなった場合は警告のみが表示され、makeの処理は続行される。

```make
.PHONY: fail
fail:
	-false
	echo "OK!"
```

```sh
$ make fail
false
make: [fail] Error 1 (ignored)
echo "OK!"
OK!
```

## 関数
GNU Makeには、いくつかの関数が用意されている。関数は`$(func-name arg1,arg2,...,argN)`の形で呼び出す。以下は代表的な関数の例である:

+ `$(subst from,to,text)`: `text`内のすべての文字列`from`を`to`に置換する。
+ `$(patsubst fromp,top,text)`: `text`内の、パターン`fromp`にマッチする部分をパターン`top`に置換する。
+ `$(firstword words)`: スペース区切りの単語列`word`の先頭の単語を返す。
+ `$(dir paths)`: スペース区切りのパスの列`paths`の各要素から、末尾のファイル名を取り除いたものを返す。
+ `$(wildcard pat)`: `pat`内のワイルドカードを展開する。
+ `$(forearch var,list,text)`: 単語列`list`の各単語を先頭から順に`var`に束縛した状態で`list`を評価したものを返す。
+ `$(if cond,then,else)`: `cond`が空文字列なら`else`を、そうでないなら`then`を返す。

すべての関数の一覧は[公式のマニュアル](https://www.gnu.org/software/make/manual/html_node/Functions.html#Functions)を参照。

### ユーザー定義関数
ユーザー定義関数とは、実際にはただの変数である。`$(call USERFUNC,arg1,...,argN)`を評価すると、`$1`,...,`$N`にそれぞれ`arg1`,...,`argN`が束縛された状態で変数`USERFUNC`が展開される。

```make
HELLO = Hello, $(1)!

.PHONY: hoge
hoge:
	echo $(call HELLO,World)
```

```sh
$ make hoge
echo Hello, World!
Hello, World!
```

## その他

### コメント

`#`から行末まではコメント

```make
# 必要なオブジェクトファイル
OBJS = hoge.o fuga.o foo.o bar.o
```

### 行継続

行末にバックスラッシュ(`\`)を置くと、そこから次の行までが一つの行とみなされる。
