---
layout: post
title: Elixirで定数値を扱う
tags:
- Elixir
---

定数値は`@`属性で扱うとよい。

``` elixir
defmodule MyModule do
  @constant_value "hoge"

  def some_method(), do: @constant_value
end
```

さらに、これを`config.exs`に保存しておくのがベストプラクティスっぽい。

``` elixir
# config/config.exs
use Mix.Config

config :my_application,
    constant_value: "hoge"
```

``` elixir
defmodule MyModule do
  @constant_value Application.get_env(:my_application, :constant_value)
end
```

