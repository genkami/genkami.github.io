---
layout: post
title: git commitを使わずにコミットを作ってみる
tags:
- Git
---

`git commit` や `git commit-tree` を使わずにコミットを作ってみます。

## 完成後のイメージ

こんな感じのコミット履歴になれるようにコミットオブジェクトを手作りしていきます。

```
$ pwd
/path/to/my-first-repository
$ ls
hello.txt
$ cat hello.txt
HELLO WORLD
$ git log --oneline
XXXXXXX (HEAD -> master) fix typo (s/HALLO/HELLO/)
YYYYYYY add hello.txt
```

最初のコミットで `hello.txt` というファイルを作り、次のコミットでそのtypoを直していくという想定になっています。

## 初期化

コミットを手作りする前に、先に `git init` だけやっておきます。

``` sh
$ mkdir my-first-repository

$ cd my-first-repository

$ git init
Initialized empty Git repository in /path/to/my-first-repository/.git/
```

## blobを作る

まずはhello.txtの最初の中身となるblobを作っていきます。

Gitは内部的にはSHA1をキーとするKVSみたいなものを持っており、すべてのファイルの中身はこのKVSのようなものにblobオブジェクトという種類で登録されます。

オブジェクトを作るには、 [git hash-object](https://git-scm.com/docs/git-hash-object)というコマンドを使います。

``` sh
$ echo 'HALLO WORLD' | git hash-object -w --stdin
a479c095720051370f567216bb5d89b300edc3cd
```

`a479c095720051370f567216bb5d89b300edc3cd` がこのオブジェクトのSHA1です。

## treeを作る
ファイルの中身ができたら、次はファイルを収めるディレクトリに対応するオブジェクトを作ります。

ディレクトリはGitではtreeオブジェクトという種類のオブジェクトとして表されます。

treeオブジェクトは、各行に `permission type SHA1\tpath` という形でファイルのメタデータが並んだ形式で表現されます。

また、treeオブジェクトは `git hash-object -t tree` で作ることができます。

``` sh
$ hex2oct () {
        perl -ne 'printf "\\%03o", hex for /../g'
}
$ printf "100644 hello.txt\0$(echo a479c095720051370f567216bb5d89b300edc3cd | hex2oct)" | git hash-object -t tree -w --stdin
cc5eabd265414052d23c2a355c7fbb9ecd7d2203
```

ここではSHA1のバイナリ表現を簡単に生成するために、hex2octというコマンドを[Gitのspec](https://github.com/git/git/blob/6a6c0f10a70a6eb101c213b09ae82a9cad252743/t/test-lib-functions.sh#L1244)より引用しています。

## commitを作る

ファイルとそれを収めるディレクトリができたら、最後にコミットを作成します。コミットオブジェクトは以下のような形式になっています。

```
parent (親コミットのSHA1、ルートのコミットの場合は無し)
tree (コミットのルートディレクトリのtreeオブジェクトのSHA1)
author (authorの名前 <email> タイムスタンプ)
committer (committerの名前 <email> タイムスタンプ)

コミットメッセージ
```

また、コミットおぶは `git hash-object -t commit` で作ることができます。

``` sh
$ echo "tree cc5eabd265414052d23c2a355c7fbb9ecd7d2203
author Hoge Taro <hogefuga@example.com> 1557821403 +0900
committer Hoge Taro <hogefuga@example.com> 1557821403 +0900

add hello.txt
" | git hash-object -t commit -w --stdin
0d9a4e89e7a9cda92bee74c8d2055006ab8c39dc
```

`tree` で指定されるtreeの中身を内容として持つコミットオブジェクトを作成しました。
ちなみに `1557821403` はこれを書いている時点での現在時刻です。

## 子コミットを作る

次に、今作ったコミット `0d9a4` を親として持つコミットを作ります。
`0d9a4` は `hello.txt` の中身を `HALLO WORLD` とtypoしてるので、これを `HELLO WORLD` に修正したコミットを作ります。

まずは先程と同様にblobから

``` sh
$ echo 'HELLO WORLD' | git hash-object -w --stdin
4e3dffe834ac70600a7cb71fbc1f6a694c9d041f
```

次にtree

``` sh
printf "100644 hello.txt\0$(echo 4e3dffe834ac70600a7cb71fbc1f6a694c9d041f | hex2oct)" | git hash-object -t tree -w --stdin
77808726e703c5f4d7394d735ad02032e2f43202
```

最後にcommit。今回は `0d9a4` に対する修正という設定なので、この `0d9a4` を親commitとして指定します。

``` sh
$ echo "tree 77808726e703c5f4d7394d735ad02032e2f43202
parent 0d9a4e89e7a9cda92bee74c8d2055006ab8c39dc
author Hoge Taro <hogefuga@example.com> 1557821600 +0900
committer Hoge Taro <hogefuga@example.com> 1557821600 +0900

fix typo (s/HALLO/HELLO/)
" | git hash-object -t commit -w --stdin
39cc518115462d6b8ea5e2ba30e92890170de705
```

## 完成

最後にmasterの位置を今できた `39cc51` に移動させれば完成です。

``` sh
$ git reset --hard 39cc518115462d6b8ea5e2ba30e92890170de705
HEAD is now at 39cc518 fix typo (s/HALLO/HELLO/)
```

`git log` を見てみるとちゃんとコミットが作成されていることがわかります。

``` sh
$ git log -p
commit 39cc518115462d6b8ea5e2ba30e92890170de705 (HEAD -> master)
Author: Hoge Taro <hogefuga@example.com>
Date:   Tue May 14 17:13:20 2019 +0900

    fix typo (s/HALLO/HELLO/)

diff --git a/hello.txt b/hello.txt
index a479c09..4e3dffe 100644
--- a/hello.txt
+++ b/hello.txt
@@ -1 +1 @@
-HALLO WORLD
+HELLO WORLD

commit 0d9a4e89e7a9cda92bee74c8d2055006ab8c39dc
Author: Hoge Taro <hogefuga@example.com>
Date:   Tue May 14 17:10:03 2019 +0900

    add hello.txt

diff --git a/hello.txt b/hello.txt
new file mode 100644
index 0000000..a479c09
--- /dev/null
+++ b/hello.txt
@@ -0,0 +1 @@
+HALLO WORLD
```

