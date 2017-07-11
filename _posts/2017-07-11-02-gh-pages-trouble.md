---
layout: post
title: Github Pagesが更新されなくなった
tags:
- Jekyll
---

昨日あたりから突然、Github Pagesが更新されなくなりました。

以下のURLにアクセスしてリポジトリの設定を見てみると…

```
https://github.com/username/username.github.io/settings
```

以下のようなシンプルな説明だけが出ていました。

> Your site is having problems building: Page build failed. For more information, see https://help.github.com/articles/troubleshooting-github-pages-builds/.

しかしそれ以上の情報が何も出ていません…・

ちなみに、ローカルのJekyllでは問題なく動いています。

もしかしてGithub Pagesで使われているJekyllのバージョンに変化があり、その影響でエラーが起きるようになったのでは無いかと思い、以下を実行してみました。

```
$ bundle update
$ bundle exec jekyll s
...
Liquid Exception: undefined method `encoding' for nil:NilClass in /_layouts/post.html
jekyll 3.4.5 | Error:  undefined method `encoding' for nil:NilClass
```

案の定、アップデート後のJekyllがエラーをはいてくれるようになりました。

`--trace`をつけて実行してみると、`cgi_escape`の実行中に死んでいることがわかりました。

そこで調べてみると、`_includes`の中に

```html
{{ page.url | cgi_escape}}
```

みたいなやつを書いているところが何個かあったので、これを`nil`のときでも大丈夫なように、

```html
{{ page.url | default: "" | cgi_escape}}
```

とすることによって、動くようになってくれました。
