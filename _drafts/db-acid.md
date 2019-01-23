---
layout: post
title: ACID特性とトランザクションの分離レベル
tags:
- DB
---

ACID特性

説明は http://www.atmarkit.co.jp/ait/articles/1703/01/news194.html より引用

* 原子性(Atomicity): トランザクションは完全に実行されるか、全く実行されないかのどちらかでなければならない。
* 一貫性(Consistency): トランザクションの終了状態にかかわらず、データベースの整合性が保たれなければならない。
* 独立性(Isolation): トランザクションを複数同時に実行しても、単独実行の場合と同じ処理結果にならなければならない。
* 耐久性(Durabiliby): トランザクションの結果は、障害が発生しても失われてはいけない。


整合性ってなんだ？
[Consistency (database systems) - Wikipedia](https://en.wikipedia.org/wiki/Consistency_(database_systems))
これによると若干曖昧さはあるけどだいたいこんな感じ
* トランザクションは、それが始まるよりも前にコミットされたトランザクションの影響を受ける。
* データベースの制約(NOT NULLとかFOREIGN KEYとか)が破られない。
* The guarantee that operations in transactions are performed accurately, correctly, and with validity, with respect to application semantics
application semanticsって何だ？自然言語的な意味でいいのかな？それともなんかの用語？


2相コミット
https://ja.wikipedia.org/wiki/2%E7%9B%B8%E3%82%B3%E3%83%9F%E3%83%83%E3%83%88
https://www.ogis-ri.co.jp/otc/hiroba/technical/DTP/step2/index.html

単に順番にcommitするだけで、なんとなく整合性を保ってるつもりになってること多そう

MySQL XA Transaction
https://dev.mysql.com/doc/refman/5.6/ja/xa.html
