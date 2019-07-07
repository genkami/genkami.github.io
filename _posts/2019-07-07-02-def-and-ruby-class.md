---
layout: post
title: defでメソッドが定義される対象
tags:
- Ruby
---

`def` キーワードを使うとメソッドを定義できますが、実は文脈によって挙動が結構違います。

## 特異メソッド
誰にメソッドが追加されるかを明示している特異メソッド定義の場合は挙動がわかりやすいです。この場合は、単純に対象のオブジェクトの特異クラスにメソッドが定義されます。

```ruby
hoge = "hoge"
def hoge.puts_self
  puts self
end
hoge.puts_self #=> hoge
```

## トップレベル
トップレベルでの `def` では、 `Object` のプライベートメソッドとして定義されます。

また、トップレベルでは `self` が `main` オブジェクトという特別な `Object` のインスタンスになるので、定義したメソッドをそのまま関数のように呼ぶことができます。

```ruby
def puts_self
  puts self
end

puts_self #=> main
Object.new.send(:puts_self) #=> #<Object:0x00007fc336858d00>
```

## クラス定義の内部
クラス定義の内部では、現在定義しようとしているクラスにメソッドが定義されます。

``` ruby
class TheClass
  def puts_self
    puts self
  end
end

TheClass.new.puts_self #=> #<TheClass:0x00007ff2d8868440>
```

## class_eval
`class_eval` では、クラス定義の内部と同じように、レシーバのクラスにメソッドが定義されます。

``` ruby
class AnotherClass
end

AnotherClass.class_eval do
  puts self #=> AnotherClass
  def puts_self
    puts self
  end
end
AnotherClass.new.puts_self #=> #<AnotherClass:0x00007fb538113740>
```

## instance_eval
`instance_eval` は `class_eval` と違い、レシーバの特異クラスに対してメソッドが定義されます。

注意点として、 `class_eval` と `instance_eval` ではともにそのコンテキストでの `self` はレシーバ自身になります。

``` ruby
AnotherClass.instance_eval do
  puts self #=> AnotherClass
  def puts_self
    puts self
  end
end
AnotherClass.puts_self #=> AnotherClass
```

