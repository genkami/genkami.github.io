---
layout: post
title: CockroachDB
tags:
- DB
---

https://www.cockroachlabs.com/

これもGoogleのF1の論文を元にしてる？

TiDBに近そうな気がする。今度調べる。


https://www.cockroachlabs.com/docs/stable/frequently-asked-questions.html

これ読んだ感じ、バックエンドがKVS, Raft, SQLを喋るっていうところまでTiDBと同じ。MySQLじゃなくてPostgreSQLのSQL文を喋るっていうところは微妙に違う。

CAP定理のC, A, Pのうちどれを犠牲にしてるか、って点で今後いろいろ比較してみたい。

https://dbdb.io/db/cockroachdb
> The SQL layer sits on top of the transactional and strongly-consistent distributed key-value store. In the key-value store, the key ranges are divided and stored in RocksDB and replicated across cluster.

だいたい同じにみえる…。バックエンドに使ってるのもTiDBと同じRocksDB

https://ameblo.jp/principia-ca/entry-11968223741.html

構造の違いとかはあるのかな？まだTiDBも理解してないのでわからない
