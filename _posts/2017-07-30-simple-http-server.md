---
layout: post
title: 一行でHTTPサーバーを立てる
tags:
- Others
---

静的コンテンツだけのWebサイトの見た目をちょこっと確認するときとかに便利です。

```sh
$ python -m http.server 8080
```

Python2系では`SimpleHTTPServer`とかいうモジュール名だったはず
