---
layout: post
title: Rubyのフリップフロップ
tags:
- Ruby
---

この記事は[過去のブログ](http://monamonamonad.github.io/2017/03/16/ruby-flipflop.html)から移行したものです。


謎機能。Perlやらsed, awkにもあるらしい。

``` ruby
irb(main):021:0> (1..10).each {|i| puts i if i==3..i==5 }
3
4
5
=> 1..10
```

フリップフロップはifの条件部分やwhile等のみに現れ、`..`の左の式が`true`になった場合
式全体が`true`となり、`..`の右の式が`true`になるまで`true`であり続ける。
その後は`false`となる。

`..`のかわりに`...`も使える。こちらは右が`true`になった瞬間に`false`になる。