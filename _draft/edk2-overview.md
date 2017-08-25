---
layout: post
title: 
tags:
- 
---

dsc
Platform Description
Platformの定義
Platformとは？
iniファイル的書式
複数のライブラリもまとめたかなりでかい単位
本来はファームウェアのデータを作るためにある？

LibraryClasses
EDK2ではライブラリをその名前と実体に分けている
dscファイルでライブラリの実体がどこにあるのかを指定する
infからはライブラリ名での指定を行うはず

EDK II DSC files are a list of:
EDK II Module INF Files
EDK Components
EDK libraries (only used by EDK Components)
EDK II Library Class Instance Mappings (only used by EDK II Modules)
EDK II PCD Entries

(PCD: Platform Configuration Database. プログラム全体で使う設定系の値)
使用するfdfファイルもdscファイルの[Defines]のFLASH_DEFINITIONで指定する
firmware volumeをビルドするわけでないならfdfの指定は必要ない

dscファイルはビルドするファイルの指定、fdfファイルはそれがどのようにfirmware volume imagesに入れられるかを指定する

dscとかdecとかfdfとかinfとかを混ぜ合わせてMakefileとautogen.c, autogen.hが作られるっぽい

fdf
FDFファイルはファームウェアのフラッシュデバイスの中身を決めるのに使われているっぽい

inf
Module Information File
モジュール単位
ビルドに必要なソースコードの一覧
依存しているライブラリ
依存しているパッケージ
使用するプロトコルのGUID

dec
infで参照するGUIDの定義もここに書くらしい
ここで定義されたGUIDのうち、infファイルが要求したものの定義がAutogen.hに書かれ、自動でincludeされる
LibraryClasses
decではライブラリが提供するヘッダファイルのパス
各ライブラリは一つのヘッダファイルしか持てない？
確かにそんなこと.infの説明に書いてた気がする

Pkg
バイナリとかソースの集合
複数のパッケージをまとめて一つのパッケージにできるらしい

UEFI Application と UEFI Driver の違い
UEFI DriverはEFI_SUCCESSを返せば実行終了後もメモリに残り続けるが、Applicationはメモリ上から削除される。それだけ。
