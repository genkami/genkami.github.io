---
layout: post
title: Rubyのthrow/catchとraise/rescueの違い
tags:
- Ruby
---


## throw/catch

`throw`/`catch`は`Kernel`のメソッド。

```ruby
# Kernel のメソッドであることがわかるように、明示的に Kernel.(...) の形で呼んでいる
Kernel.catch :error do
  puts "hoge"
  Kernel.throw :error
  puts "fuga"
end
#=> hoge
```

`throw`で投げることができるのは任意のオブジェクト。`throw`で投げたものが`catch`で指定したものと`equal?`の意味で等しければ、`catch`の引数にあるブロックから脱出する。


## raise/rescue
こっちが通常の例外処理に使われる。
`raise`は`Kernel`のメソッドだが、`rescue`と後述する`begin`, `else`, `ensure`はメソッドではないRubyの構文。

``` ruby
begin
  puts "hoge"
  Kernel.raise RuntimeError, "an error occurred"
rescue RuntimeError
  puts "error caught"
else
  puts "no error"
ensure
  puts "this must be evaluated"
end
#=>
# hoge
# error caught
# this must be evaluated
```

## 参考

+ [制御構造 (Ruby 2.5.0)](https://docs.ruby-lang.org/ja/2.5.0/doc/spec=2fcontrol.html)
+ [module Kernel (Ruby 2.5.0)](https://docs.ruby-lang.org/ja/2.5.0/class/Kernel.html)
