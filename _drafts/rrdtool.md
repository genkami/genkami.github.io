---
layout: post
title: RRDToolの概要と、メトリクスを取得して簡単なグラフを描画するまで
tags:
- Linux
---

時系列データを保存したり、保存した時系列データをもとにグラフを描画したりするのに使われる古のツール。

### 用語
#### Round Robin Database (RRD)
RRDは、時系列データを保存するデータベースです。後述する `rrdtool create` コマンドで作成することができます。

#### Data Source (DS)
DSは、RRDに保存されるデータの取得方法です。どのような種類のメトリクスが、どれくらいの感覚で記入されるのかを定義します。実際のメトリクスの記入は、後述する `rrdtool update` コマンドで行います。

DSは、 `rrdtool create` コマンド内で以下のような書式で定義することができます。

```
DS:<ds-name>:<DST>:<dst arguments>
```

ここで、 `<ds-name>` はデータソースの識別子となる1-19文字の英数字です。 `<DST>` はデータソースの種類を表し、例えば以下のようなものがあります:

* `GAUGE` : 取得した値をそのまま使う
* `COUNTER` : 前回の結果に加算され、増え続ける値
* `DERIVE` : 前回との差分

また、 `<dst arguments>` は `<DST>` に対する引数を意味します。上で紹介した `GAUGE`, `COUNTER` の場合は、以下のような書式になります:

```
<heartbeat>:<min>:<max>
```

ここで `<heartbeat>` は、データの更新の間が何秒以上空くと、その時点での値を未定義値 `*UNKNOWN*` とみなすかを、 `<min>` と `<max>` はデータの最小値と最大値を表します。 `<min>`, `<max>` は特に指定したくない場合、未定義を表す `U` を記入することができます。

#### Round Robin Archive (RRA)
RRAは、データソースの保存方法の定義を表します。RRDToolではデータはラウンドロビン形式で保存されており、どの程度の期間のデータを残すかや、どの程度の期間で集計したデータを残すかをRRAによって定義します。

RRAは後述する `rrdtool create` コマンドで以下のように定義します:

```
RRA:<CF>:<cf arguments>
```

ここで `<CF>` は consolidation function の略で、集計に用いる関数を表します。 consolidation function は例えば以下のようなものがあります:

* `AVERAGE` : 平均値
* `MIN` : 最小値
* `MAX` : 最大値

また、 `<cf arguments>` は consolidation function に与える引数で、上の例で紹介した `consolidation function` の場合、以下のような形式になります:

```
<xff>:<steps>:<rows>
```

ここで、 `<xff>` はどれくらいの割合のデータが `*UNKNOWN*` であれば集計結果も `*UNKNOWN*` とみなすか、 `<steps>` は直前何個のデータで集計をするか、 `<rows>` は直前何個の集計結果を保存しておくかを定義します。また、 [これらは個数の代わりに時間で指定することもできます](https://oss.oetiker.ch/rrdtool/doc/rrdcreate.en.html#STEP%2C_HEARTBEAT%2C_and_Rows_As_Durations) 。

### RRDの作成
RRDは、 `rrdtool create` コマンドで作成することができます。コマンドの書式は以下のようになります:

```
$ rrdtool create DESTFILE \
    --start START_TIME \
    --step STEPS \
    'DS:<ds-name1>:<DST1>:<dst arguments1>' \
    'DS:<ds-name2>:<DST2>:<dst arguments2>' \
    ...
    'DS:<ds-nameN>:<DSTN>:<dst argumentsN>' \
    'RRA:<CF1>:<cf arguments1>' \
    'RRA:<CF2>:<cf arguments2>' \
    ...
    'RRA:<CFM>:<cf argumentsM>
```

### データの保存

### グラフの描画

### 例



### 参考
* https://oss.oetiker.ch/rrdtool/doc/rrdcreate.en.html
* http://rrdtool.vandenbogaerdt.nl/tutorial/rrdcreate.php

