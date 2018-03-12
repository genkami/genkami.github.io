---
layout: post
title: 【2018年版】MacBookを買ったら入れるべきアプリと初期設定
tags:
- Mac
---

新しいMacBook Airを手に入れたので、最初にインストールしたものとか初期設定とかをまとめておきました。これさえあればそれなりに使えるようになるはず……。

## Firefox
Rustを応援したいのでFirefox派です。

[公式](https://www.mozilla.org/ja/firefox/)からダウンロード

## Google日本語入力

[公式](https://www.google.co.jp/ime/)からダウンロード

入力ソース大量にあってもあんまり使わないので、Google日本語入力の英数とひらがなの2種類だけ残し、他は削除。

### 入力の切り替えをCommand+Spaceにする方法

「システム環境設定」→「キーボード」→「ショートカット」→「入力ソース」の「前の入力ソースを選択」のキーを「⌘スペース」に変更。Spotlightのキーと被るけどそっちは使ったことないので無視。

### 英字のスペルチェックを消す

「システム環境設定」→「キーボード」→「ユーザー辞書」にスペルチェック関連の設定があるので、全てのチェックを外す。

## iTerm2
これがないとお話にならない。

[公式](https://www.iterm2.com/)からダウンロードし、インストール

### ウィンドウの分割設定を保存する
通常は4分割して使ってるので、デフォルトでそうなってくれると嬉しい。以下の設定でウィンドウの分割を保存できます。

1. 「Window」→「Save Window Arrangement」で適当に名前を付けて保存
2. 「iTerm2」→「Preferences...」→「General」の「Startup」を「Open Default Window Arrangement」に変更。
3. 「iTerm2」→「Preferences...」→「Arrangements」で、先ほど保存したウィンドウの配置をデフォルトに設定。

## Homebrew
[公式](https://brew.sh/index_ja.html)からインストール

```sh
$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

## Xcode コマンドラインデベロッパーツール
`gcc`とかそのへんの存在しないコマンドを適当に叩くと「コマンドがありません」→「Xcodeのデベロッパーツールをインストールしますか？」みたいな流れでインストールする流れにさせられるので、流されておきましょう。

## git
Homebrewから入れるgitのほうには`git-completion.bash`が付属しています。

```sh
$ brew install git
```

## hub
Github上のリポジトリの操作を楽にしてくれるGitの補助みたいなやつ。地味に便利。

```sh
$ brew install hub
```

## Emacs
日本語環境だと[emacsmacport](https://github.com/railwaycat/homebrew-emacsmacport)が良さそう。

```sh
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac
$ brew linkapps
```

デフォルトでインストールされているEmacsのほうが`PATH`的に優先されるはずなので、そこの対処をしておくと便利かもしれません。

## 公開鍵/秘密鍵の生成、登録
`ssh-keygen`で鍵を作った後、[Github](https://github.com/)や[BitBucket](https://bitbucket.org/)などに登録。

## ホスト名の変更
前に書いた通り。「システム設定」→「共有」と、`scutil`の二ヶ所で変更を行います。

[Macで$HOSTNAMEとhostnameを一致させる - Сегодня я узнал](https://genkami.github.io/2017/06/23/03-mac-host-name.html)

## *env系
pyenv, rbenvとかとその仲間たち

```sh
$ brew install haskell-stack
$ brew install ocaml opam && opam init
$ brew install pyenv pyenv-virtualenv
$ brew install nodenv
$ brew install erlang elixir-build exenv
$ brew install rbenv && gem install bundler
```

## Docker
[公式](https://docs.docker.com/docker-for-mac/install/#download-docker-for-mac)からダウンロード&インストールした上で、とりあえずログイン。

```sh
$ docker login
```

## Google Cloud SDK
基本的に[公式](https://cloud.google.com/sdk/docs/?hl=ja)のドキュメントに従ってインストールすればいい。ただし、使用できるPythonは2.7のみ。さらに、`pyenv`上の環境でインストールするには`CLOUDSDK_PYTHON_SITEPACKAGES`環境変数を指定しなければならない。

```sh
$ tar -xf google-cloud-sdk-XXX.tar.gz
$ cd google-cloud-sdk
$ CLOUDSDK_PYTHON_SITEPACKAGES=1 ./install.sh
$ ./bin/gcloud auth login
```

詳細はこの記事を参照: [pyenv上のPythonを使ってGoogle Cloud SDKをインストールする - Сегодня я узнал](https://genkami.github.io/2017/06/25/01-gcloud.html)

## Heroku

```sh
$ brew install heroku/brew/heroku
$ heroku login
```

## Last.fm Scrobbler
[公式](https://www.last.fm/ja/about/trackmymusic)からダウンロード&インストール

「設定」→「デバイス」→「デバイスでのScrobbleを有効にする」のチェックを外すと、曲が二重にScrobbleされるのを防ぐことができる。

## Slack
App Storeから

## Evernote
App Storeから

## Trello
App Storeから

## LINE
App Storeから

## Tweetdeck
App Storeから

## Dropbox
[公式](https://www.dropbox.com/ja/downloading)からダウンロード&インストール。

## フォント
caskroom/fontsを使うと、フォントを楽にインストールできる。

```sh
$ brew tap caskroom/fonts
$ brew cask install font-ricty-diminished # Ricty Diminished をインストール
```

## 自動化
Ansibleとか使って自動化するべき…？

でも数年に一回しかしないようなことに使うのもやりすぎ感が…
