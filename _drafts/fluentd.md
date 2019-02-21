---
layout: post
title: Fluentdを使ってみる
tags:
- Linux
- Fluentd
---

## Fluentdとは？

ログを受信、変換、送信するためのツール

## 使ってみる

お試しで使うだけならこんな感じの `Gemfile` を用意して `bundle install` すればおわり。

``` ruby
# frozen_string_literal: true

source "https://rubygems.org"

git_source(:github) {|repo_name| "https://github.com/#{repo_name}" }

# gem "rails"

gem "fluentd", "~> 1.2"
```

以下のような設定ファイルを用意してみる。
LTSV形式のファイルを `tail -F` (に近いことを)して、内容を標準出力に出すだけの設定。

``` xml
<source>
  @type tail
  path /path/to/hoge.log
  pos_file /path/to/hoge.log.pos
  tag example.tail
  format ltsv
</source>

<match example.tail>
  @type stdout
</match>
```

この設定ファイルを作った上で、

``` sh
$ bundle exec fluentd -c /path/to/fluentd.conf
```

で起動。これで `/path/to/hoge.log` の変更を監視してくれる。

``` sh
echo -e "user:John\tmessage:Hello World" >> /path/to/hoge.log
```

とかやると、Fluentd側で

```
2019-02-21 19:52:04.069967000 +0900 example.tail: {"user":"John","message":"Hello World"}
```

というようにログの内容が出力されているのがわかる。


## ログの振り分け

Fluentdではログにタグを付け、タグの内容によって処理を振り分ける。
基本的には `<source>` で読み取ったログの内容にタグを付け、 `<match>` で指定したタグに引っかかったものをどこかに出力する、というような使い方になる。

先程の例では `hoge.log` から読んだ内容に `example.tail` という名前を付け、 `<match example.tail>` で同様のタグのついているログを標準出力に出すようにしている。

以下のように複数の入力に対して別々なタグを付け、それらに対して出力先を変えるようなことができる。

``` xml
<source>
  @type tail
  path /path/to/hoge.log
  pos_file /path/to/hoge.log.pos
  tag example.tail.hoge
  format ltsv
</source>

<source>
  @type tail
  path /path/to/fuga.log
  pos_file /path/to/fuga.log.pos
  tag example.tail.fuga
  format ltsv
</source>

<match example.tail.hoge>
  @type stdout
</match>

<match example.tail.fuga>
  @type null
</match>
```

`@type null` は何もしないやつ。
例えばこんな感じの設定ファイルでFluentdを起動して

``` sh
$ echo -e "user:John\tmessage:Hello World" >>/path/to/hoge.log
$ echo -e "user:John\tmessage:Hello World" >/path/to/fuga.log
```

というようにログを吐くと、Fluentdの出力は以下のようになっている

```
2019-02-21 20:07:51 +0900 [info]: #0 following tail of /path/to/hoge.log
2019-02-21 20:07:51 +0900 [info]: #0 fluentd worker is now running worker=0
2019-02-21 20:07:53.372069000 +0900 example.tail.hoge: {"user":"John","message":"Hello World"}
2019-02-21 20:08:04 +0900 [info]: #0 following tail of /path/to/fuga.log
# (その後なにも無し…)
```

これは `hoge.log` の内容には `example.tail.hoge` というタグが割り振られ、 `fuga.log` には `example.tail.fuga` というタグが割り振られて、それぞれのタグに合わせたoutputが実行されているため。

## Filter
