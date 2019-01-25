---
layout: post
title: CockroachDB
tags:
- DB
---

https://www.cockroachlabs.com/

これもGoogleのF1の論文を元にしてる？

TiDBに近そうな気がする。今度調べる。


[faq]: https://www.cockroachlabs.com/docs/stable/frequently-asked-questions.html

[これ][faq]読んだ感じ、バックエンドがKVS, Raft, SQLを喋るっていうところまでTiDBと同じ。MySQLじゃなくてPostgreSQLのSQL文を喋るっていうところは微妙に違う。

CAP定理のC, A, Pのうちどれを犠牲にしてるか、って点で今後いろいろ比較してみたい。

https://dbdb.io/db/cockroachdb
> The SQL layer sits on top of the transactional and strongly-consistent distributed key-value store. In the key-value store, the key ranges are divided and stored in RocksDB and replicated across cluster.

だいたい同じにみえる…。バックエンドに使ってるのもTiDBと同じRocksDB

https://ameblo.jp/principia-ca/entry-11968223741.html

構造の違いとかはあるのかな？まだTiDBも理解してないのでわからない


TiDBとの違い
https://news.ycombinator.com/item?id=15500582
* MySQL vs PostgreSQL
* TiDBはSQLのレイヤーとKVSのレイヤーが完全に分離してる。どっちか片方だけを使うことも可能。 [これ][arch] だけみるとCockroachDBもレイヤーわけされてそうだけどそういうことではなく？
* TiDBは時刻をすごい正確に測れる必要がある(必要な理由はSpannerの論文に書いてる)。CockroachDBではすごい正確な時計がなくてもなんとかなるように作られてる？詳細は[clocksync][]を参照。
* CockroachDBは全部Goだけど、TiDBはSQLレイヤーをGo, KVSレイヤーをRustで書いてる。その分後者のほうが速くなるかも？

[arch]: https://www.cockroachlabs.com/docs/stable/architecture/overview.html
[clocksync]: https://www.cockroachlabs.com/docs/stable/recommended-production-settings.html#clock-synchronization


https://news.ycombinator.com/item?id=15499404
この議論けっこう参考になる。
