---
layout: post
title: Dockerのコンテナを全部削除する
tags:
- Others
---

`docker ps -qa`でコンテナID一覧のみを取得できます。

```
$ docker ps -qa | xargs docker rm
```
