---
layout: post
title: Vitess
tags:
- DB
---

https://github.com/vitessio/vitess
Youtubeが使ってるやつ。たしか。

トランザクションはサポートしてるぽい。でもACID特性を満たすようなシャード間のトランザクションはサポートしてないってCockroachあたりが言ってた気がする。


TiDBとかCockroachDBとかと比べると、どちらかというとMySQLをクラウドネイティブの世界でいい感じにシャーディングしてくれるだけのやつっていう側面が強そう。

MySQLをいい感じにシャーディングしてくれるやつっぽいけどインターフェースは完全にMySQLと一緒なわけではないらしい。gRPCで喋る？
