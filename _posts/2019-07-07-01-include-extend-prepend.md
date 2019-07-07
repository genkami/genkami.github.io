---
layout: post
title: よく忘れるinclude, prepend, extend
tags:
- Ruby
---

Rubyの `include`, `prepend`, `extend` のどれがどれだかすぐに忘れるのでまとめました。

## include
`Module#include` は、自身の継承チェーンの「自身より後、自身のスーパークラスより前」に指定したモジュールを挿入するメソッドです。

結果として、 `include` したモジュールに定義されているメソッドをそのままインスタンスメソッドとして取り込むことができます。

モジュールを継承？と思うかもしれませんが、Rubyの場合クラスはただのインスタンス化できるモジュールであることを思い出してください。

例:

```ruby
class Parent
  def hello
    puts "Parent#hello"
  end
end

class TheClass < Parent
  def hello
    puts "TheClass#hello"
  end
end

module Included
  def hello
    puts "Included#hello"
  end

  def goodbye
    puts "Included#goodbye"
  end
end

TheClass.include Included

puts TheClass.ancestors.to_s #=> [TheClass, Included, Parent, Object, Kernel, BasicObject]

instance = TheClass.new
instance.hello #=> TheClass#hello
instance.goodbye #=> Included#goodbye
```

## prepend
`Module#prepend` は、自身の継承チェーンの「自身より前」に指定したモジュールを挿入するメソッドです。

結果として、元々のクラスが持っているインスタンスメソッドを `prepend` したモジュールのメソッドで上書きすることができます。

例:

``` ruby
class Parent
  def hello
    puts "Parent#hello"
  end
end

class TheClass < Parent
  def hello
    puts "TheClass#hello"
  end
end

module Included
  def hello
    puts "Included#hello"
  end

  def goodbye
    puts "Included#goodbye"
  end
end

module Prepended
  def hello
    puts "Prepended#hello"
  end

  def goodbye
    puts "Prepended#goodbye"
  end
end

TheClass.include Included
TheClass.prepend Prepended

puts TheClass.ancestors.to_s #=> [Prepended, TheClass, Included, Parent, Object, Kernel, BasicObject]
instance = TheClass.new
instance.hello #=> Prepended#hello
instance.goodbye #=> Prepended#goodbye
```

## extend
`Object#extend` は、そのオブジェクトの特異クラスに対する `include` です。

結果として、特定のインスタンスだけにメソッドを追加したり、クラスメソッドを追加するようなことができます。

``` ruby
class Parent
  def hello
    puts "Parent#hello"
  end
end

class TheClass < Parent
  def hello
    puts "TheClass#hello"
  end
end

module Included
  def hello
    puts "Included#hello"
  end

  def goodbye
    puts "Included#goodbye"
  end
end

module Prepended
  def hello
    puts "Prepended#hello"
  end

  def goodbye
    puts "Prepended#goodbye"
  end
end

module Extended
  def hello
    puts "Extended#hello"
  end

  def goodbye
    puts "Extended#goodbye"
  end
end

TheClass.include Included
TheClass.prepend Prepended
TheClass.extend Extended

puts TheClass.ancestors.to_s #=> [Prepended, TheClass, Included, Parent, Object, Kernel, BasicObject]
# 注意: #<Class:hoge> となっているのは hoge の特異クラス
puts TheClass.singleton_class.ancestors.to_s #=> [#<Class:TheClass>, Extended, #<Class:Parent>, #<Class:Object>, #<Class:BasicObject>, Class, Module, Object, Kernel, BasicObject]

instance = TheClass.new
instance.hello #=> Prepended#hello
instance.goodbye #=> Prepended#goodbye

TheClass.hello #=> Extended#hello
TheClass.goodbye #=> Extended#goodbye

instance.extend Extended
puts instance.singleton_class.ancestors.to_s #=> [#<Class:#<TheClass:0x00007ff45d039888>>, Extended, Prepended, TheClass, Included, Parent, Object, Kernel, BasicObject]
instance.hello #=> Extended#hello
instance.goodbye #=> Extended#goodbye
```

注意すべき点は、 `instance.extend Extended` は `TheClass` に対する動作ではなく `instance` の特異クラスに対する動作なので、 `Extended` が挿入される位置は「`TheClass` の後、 `Parent` の前」ではなく、「`instance` の特異クラスの後、 `TheClass` の前」になります。

