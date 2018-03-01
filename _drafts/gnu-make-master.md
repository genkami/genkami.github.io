---
layout: post
title: GNU Make 基礎文法最速マスター
tags:
- Linux
---

## Makeとは?

Makeとは、ファイルの生成方法を定義するもの。

ファイルの生成とは、典型的にはCのソースをビルドして実行ファイルを作るといった感じ。

Makeに対してファイルの生成方法を指示するためのスクリプトのようなものをmakefileといい、makefileは基本的に`Makefile`というファイル名で置かれなければならない。

## 基本的な構文

makefileの基本的な構文は以下のような感じ:

```make
target1 target2 ... targetN: dep1 dep2 ... depM
	cmd1
	cmd2
	...
	cmdK
```

`target1`, ..., `targetN`をそれぞれ**ターゲット**という。

ターゲットとは、基本的にはファイル名のこと。例外として、ファイル名ではないターゲットを作ることもできる(後述)。

`dep1`, ..., `depM`をそれぞれ**前提条件**という。前提条件も基本的にはファイル名を表す。

`cmd1`, ..., `cmdK`はコマンド。基本的にはシェルスクリプトとほぼ同様の文法で書ける。コマンドの前にあるのはタブ文字でなければならないので注意(スペースは☓)。

`target1`, ..., `targetN`のそれぞれは`dep1`, ..., `depM`に依存しており、そのターゲットの生成方法は`cmd1`, ..., `cmdK`である。という意味。

このような、ターゲットを生成する方法の定義のことを、**ルール**という。

この状態で、`Makefile`のあるディレクトリ上で`make target1`を実行すると、`dep1`, ..., `depM`のルールが実行された上で、`cmd1`, ..., `cmdK`が実行され、ファイル`target1`が生成される。
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

このような`Makefile`がある状態で、`make out-file`というコマンドを実行してみる。

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

Makeは、コマンドライン引数で与えられたターゲット`out-file`(これを**ゴール**という)に対して、これを生成するために必要なルールをすべて実行する。

`file-1`, `file-2`, `out-file`を生成するためのルールが、順に実行されたことがわかる。

なお、Makeは賢いので、ターゲットと前提条件にあるファイルの更新時刻を比較して、必要な場合にのみルールの実行を行う。例えば、先ほどの環境でもう一度`make out-file`を実行した場合、以下のようなメッセージが表示され、ルールは実行されない。

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
### ワイルドカードの展開

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

一方、再帰展開変数の場合は、実際に変数が展開されるまで、右辺の変数の展開を行わない。従って、後で右辺で使われている変数の値が書き換わった場合、それを使用している再帰展開変数の値も書き換わることになる。例えば、以下のmakefileを用いて`make greet`を行うと、`I like apple`が表示される。

```make
fruit = orange
msg = I like $(fruit)

fruit = apple

.PHONY: greet
greet:
	echo $(msg)
```

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

ここで定義したデフォルトのルールに含まれるような`%`を含む文字列は **型** と呼ばれ、`%`の部分に任意の1文字以上の文字列がマッチする。

## コマンド
### シェルの変更
先ほどコマンドに関して「基本的にはシェルスクリプトとほぼ同様の文法で書ける」と書いたが、実際にコマンドは各行ごとにシェルに送られる。

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

## 再帰的なmake

## その他

### コメント

`#`から行末まではコメント

```make
# 必要なオブジェクトファイル
OBJS = hoge.o fuga.o foo.o bar.o
```

### 行継続

行末にバックスラッシュ(`\`)を置くと、そこから次の行までが一つの行とみなされる。
