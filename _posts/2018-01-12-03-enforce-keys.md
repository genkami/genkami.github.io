---
layout: post
title: Elixirでstructが特定のフィールドをもつことを強制する
tags:
- Elixir
---

`@enforce_keys`を使うことで，structが特定のフィールドを持つことを強制することができます．

例は[先ほど]({% post_url 2018-01-12-02-alias-myself %})の`Game.User`で．

```elixir
defmodule Game.User do
  alias __MODULE__

  defstruct [:name, :level]

  def new(name, level) do
    %User{name: name}
  end
end
```

よくみると，このコードには明らかなバグがあります．
struct `Game.User` は2つのフィールド `name`, `level` を持ちますが， `Game.User.new/2`が返すstructには，`level`フィールドが抜けています．

この場合，コンパイル時に警告は表示されるものの，普通に実行することができてしまいます．

```
warning: variable "level" is unused
  lib/game/user.ex:6
```


このようなとき，以下のように`@enforce_keys`属性で必須なフィールドを指定すると，そのフィールドが欠けているコードが見つかった場合コンパイル時にエラーを吐いてくれるようになります．

```elixir
defmodule Game.User do
  alias __MODULE__

  @enforce_keys [:name, :level]
  defstruct [:name, :level]

  def new(name, level) do
    %User{name: name}
  end
end
```

これをコンパイルしようとすると，以下のようにエラーが表示されるはずです．

```
== Compilation error in file lib/game/user.ex ==
** (ArgumentError) the following keys must also be given when building struct Game.User: [:level]
    (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
    lib/game/user.ex:7: (module)
```
