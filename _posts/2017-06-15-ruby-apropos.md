---
layout: post
title: Rubyでそれっぽい名前のメソッドを探す
tags:
- Ruby
---
[前のブログ](http://monamonamonad.github.io/2017/03/17/irb-setting.html)に書いたやつを少しだけ書き換えました。

正規表現を渡すと、定義されているメソッドのうちそれにマッチするものを列挙してくれるメソッドです。

``` ruby
class Kernel
  def apropos(regex)
    methods.to_a.map(&:to_s).grep ->(s){ regex.match s }
  end
end
```

`.irbrc`とかに書いておくと便利です。

``` ruby
irb(main):001:0> apropos /exit/
=> ["exit", "irb_exit"]
irb(main):002:0> "hoge".apropos /start/
=> ["start_with?"]
irb(main):003:0>
```

