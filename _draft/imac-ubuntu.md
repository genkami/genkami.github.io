---
layout: post
title: 
tags:
- 
---
refindがインストールされているのが前提

https://www.ubuntu.com/download/desktop

ダウンロード

ディスクユーティリティを開き、
USBメモリを右クリック→消去から、FATでフォーマット

https://unetbootin.github.io/
UNetbootin をダウンロードし、起動
先ほどダウンロードしたUbuntuのイメージを指定して、USBメモリに書き込む。


パーティション分割
ディスクユーティリティ
HDDを選択
「パーティション」をクリック
円グラフみたいなのが出てくるので、適当にいじる
Ubuntu本体とswapのパーティションを作成

Optionキーを押しながら起動
メニューからEFI Bootを選ぶと、Ubuntuが起動される
「ブートローダーをインストールするデバイス」はUbuntuをインストールするパーティションにする。rEFIndが勝手に認識してくれるはず。

無事インストールが終わったら再起動

間違えてrEFIndを消し飛ばし、Macが起動できなくなる

/boot/efiの中を見てみると、やっぱりrEFIndが消滅してる

http://www.rodsbooks.com/refind/installing.html

$ sudo apt-add-repository ppa:rodsmith/refind
$ sudo apt-get update
$ sudo apt-get install refind


設定ファイルなどは /boot/efi/EFI/refind/ 以下

これで完了
