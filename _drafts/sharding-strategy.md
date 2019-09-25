---
layout: post
title: いろんな会社のシャーディング事情
tags:
- DB
---

最近仕事でシャーディングをやっているので、色んな会社がどうやってシャーディングをやっているかについて以下の観点でまとめてみました。
* 何をキーとしてDBを割り振っているか
* 採番をどのように行っているか
* どのような運用でシャーディングしていない状態からシャーディングしている状態に移行しているか

## まとめ
だいたい各社こんな方法を取っているよっていうのが入る

### Instagram
https://instagram-engineering.com/sharding-ids-at-instagram-1cf5a71e5a5c
各種採番機のpros & cons がまとまっていてよい
特徴:
* (タイムスタンプ, シャード番号, テーブル内での連番 % 1024) の組を64ビット整数にエンコードしてIDとして扱っている。
* 完全な連番ではないが、時系列に沿ってidが大きくなっていく。
* ストアドファンクションでIDの採番をしている。アプリケーション側ではシャードさえ決まれば採番ロジックを意識する必要はない。
* 移行作業については書いてない


### Pinterest
https://medium.com/@Pinterest_Engineering/sharding-pinterest-how-we-scaled-our-mysql-fleet-3f341e96ca6f
特徴:
* 予め論理的に大量のシャードを作っておいて、論理シャードを別なホストに移動することでスケールアウトできる。
* (シャード番号, データの型, テーブル内の連番) の組を64ビット(正確には62ビット)整数にエンコードしてIDとして扱っている。
* 完全な連番ではないが、時系列に沿ってidが大きくなっていく。
* ↑のID以外で引っ張りたいデータは別で管理して、 `hash(何らかのデータ) % シャード数` で振り分けする。
* 多対多関係のようなマッピングテーブルは片方のidの該当するシャードに突っ込む。この方法だと一方向にしかデータを引けなくなるので、逆引きもしたい場合はもう一個テーブルを作る。
* すべてのオブジェクトに対して一意なIDが振られる。
* 論理シャードの動かし方は？
  - 既存の master-master ペア A-B の複製を1セット(A'-B')作り、以下のようにレプリケーションする
A-B
+-A'-B'
どのつながりもmaster-master replication
その後設定を変えて、Aがもつ論理シャードのうち一部はA'に書き込みをしにいくようにする。
切り替えの瞬間に両方のDBに同じidを持つデータがインサートされて整合性壊れたりしないの？

### Flickr
http://code.flickr.net/2010/02/08/ticket-servers-distributed-unique-primary-keys-on-the-cheap/
特徴:
* 採番テーブル方式。採番テーブル内でのIDの更新は `REPLACE INTO` で行っている。
* 採番テーブルがSPOFになるのを避けるため、奇数番の採番テーブルと偶数番の採番テーブルを分けている。(この方法では完全に時系列順にはならない。)

### クックパッド
https://techlife.cookpad.com/entry/2015/06/22/134108
* シャーディングに使われているmixed_gaugeというgemはspec含めて1500行程度と非常にコンパクト。
* なんかの値 `v` に対して `n = hash(v) mod N` を使って保存するシャードを選ぶ。 `n` の値と保存先のホストの対応関係だけ手動で持っておく。「なんかの値」が何なのかはこの記事からはわからなかった。
* データのコピーはマルチマスターレプリケーション
* 移行の瞬間のid衝突はauto_increment_offset, auto_increment_incrementをずらすことで回避


### Twitter
https://blog.twitter.com/engineering/en_us/a/2013/new-tweets-per-second-record-and-how.html
SnowflakeというID生成器を使って(作って)る
https://www.slideshare.net/moaikids/20130901-snowflake

* (タイムスタンプ, ID生成器のID, 生成器ごとの連番) の組を64ビット整数にエンコードしてIDとして扱っている。
* Pinterestはアプリケーションのロジックでカバー(多分)、Instagramはストアドファンクションを使ったのに対し、TwitterはID採番サーバーを立てた。
* 時間はunixtimeそのものではなく、特定の時刻(比較的最近)の時刻を0とするようにずらすことでビット数が少なくなってもある程度長い時間に対応できるようにしている。Instagramもたしかこんな感じの方法を取ってるはず。

### カヤック
https://techblog.kayac.com/6_techkayaccom_advent_calendar_2012.html
* 採番テーブル方式。 `UPDATE sequense SET id = LAST_INSERT_ID(id+1);`
* マッピングテーブル方式。
* 移行方法は書いてなかった。

### LINEマンガ
https://engineering.linecorp.com/ja/blog/line-manga-database/

