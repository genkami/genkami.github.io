---
layout: post
title: TiDB
tags:
- DB
---

TiDB
https://github.com/pingcap/tidb
水平分散するように作られたデータベース。MySQLのプロトコルを喋れる。分散しているDBに対してトランザクションが張れる。MySQLのXAトランザクションとは何が違うんだろう？シャードの存在を意識しなくていいとか？

バックエンドにKVSを使ってるっぽい。Auroraとにてたりするのかな？

特徴として
* SQL文の処理
* read
* write
* データの総容量
これらがノードを増やすだけでスケールするようになる

元になってるのはこの論文っぽい
https://ai.google/research/pubs/pub41344

MySQLとの違い
https://pingcap.com/blog/5-key-differences-between-mysql-and-tidb-for-scaling-in-the-cloud/

### クエリの実行もストレージもデフォルトで分散されてる
もちろん読み込みは分散。
Slaveをいっぱい生やすのと違って書き込みも分散できる。
multi masterと違ってストレージを分散できる。

テーブルのデータを一定サイズに収まるようにrange-baseで切り分けた塊(TiDBではRegionという)をノードに振り分ける。同じRegionを3つのnodeが持つようになっているっぽい。

### ストレージエンジンにRocksDBを使っている
https://rocksdb.org/

### デフォルトでPrometheus+Grafanaでモニタリングできるっぽい

### いい感じのDDLの変更ができる
DDL = Data Definition Language
ALTERとかをいい感じにやれる
GoogleのF1の論文に書いてる方法らしいのでやっぱりこの論文を読むしか無い

### HTAP
要するにアプリケーション的な操作と解析系の操作を同じDB上でやれちゃうよというやつらしい。



https://medium.com/@PingCAP/how-we-build-tidb-ec6c3b1a3f2


https://www.slideshare.net/morgo/tidb-introduction

以下引用
Row:
Key: tablePrefix_rowPrefix_tableID_rowID
Value: [col1, col2, col3, col4]

Index:
Key: tablePrefix_idxPrefix_tableID_indexID_ColumnsValue_rowID
Value: [null]

この構造からしてRocksDBは前方一致検索できるってことかな？まあ木構造で管理してるKVSっぽいしできそうな気はする。

