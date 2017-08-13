---
layout: post
title: 'MacでBitVisorをビルド(Docker編)'
tags:
- Mac
---

Mac上でBitVisorをビルドする方法として、考えられるのは以下のような方法があります

1. Mac上で環境を整えて頑張ってビルド  
これはかなり茨の道です。できることはできるようですが、BitVisor自体のソースコードを弄ったりする必要があり、割と面倒です。
2. Linuxをデュアルブートしてそっちでビルド  
Linuxメインで使ってる人なら問題無いでしょうが、Macメインの人はビルドのたびに再起動しなければならないので不便です。
3. VirtualBox上にLinuxを入れてそっちでビルド  
こちらは先ほどよりは楽ですが、ファイル共有の設定が結構面倒だったりします。

というわけで、今回はもう少し手軽にBitVisorをビルドするためのDockerイメージを用意することにしました。


BitVisorのビルドに必要なのは`build-essential`, `mingw-w64`あたりなので、これらのみ入った最低限のUbuntu環境を用意します。

```
FROM ubuntu:xenial

RUN apt -y update &&
    apt -y install \
        build-essential \
        mingw-w64

VOLUME /work

WORKDIR /work

CMD make
```

このイメージを使ってビルドします。ファイル共有もコマンド一発なので楽々。

こんな感じで使います。

```sh
$ hg clone https://bitbucket.org/bitvisor/bitvisor /path/to/bitvisor
$ docker build .
Successfully built XXXXXXXX
$ # BitVisor本体のビルド
$ docker run --rm -v /path/to/bitvisor:/work -it XXXXXXXX
$ # UEFI用のローダーのビルド
$ docker run --rm -v /path/to/bitvisor:/work -it XXXXXXXX make -C boot/uefi-loader
```

