---
layout: post
title: '魔法のおまじない: alias __MODULE__'
tags:
- Elixir
---

Elixirではネストされたモジュールとその親の間には何の関係もないので，ネストしたモジュール内でそのモジュール自身を参照するときは，いちいち頭からモジュール名を全部打っていかないといけません．

```elixir
defmodule Game.User do
  defstruct [:name, :level]

  def new(name, level) do
    # %User{...} ではダメ
    %Game.User{name: name, level: level}
  end
end
```

これを解決するために，以下のようにモジュール内で自分自身へのエイリアスを張ることができます．

```elixir
defmodule Game.User do
  alias Game.User

  defstruct [:name, :level]

  def new(name, level) do
    %User{name: name, level: level}
  end
end
```

こうすれば，単に`User`と書くだけで`Game.User`にアクセスできるので，少しだけ書くのが楽になります．

これをもう少しだけ楽に書くtipsとして、以下のようなものがよく使われているようです．

```elixir
defmodule Game.User do
  alias __MODULE__

  defstruct [:name, :level]

  def new(name, level) do
    %User{name: name, level: level}
  end
end
```

`Game.User`内では`__MODULE__ == Game.User`なので，これは先ほどの`alias Game.User`と意味的に同じになり，`User`が`Game.User`の意味で使えるようになるという理屈です．
