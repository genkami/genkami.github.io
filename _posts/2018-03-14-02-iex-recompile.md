---
layout: post
title: iexでモジュールを再読み込み
tags:
- Elixir
---

`recompile`でプロジェクト全体を、`r Module.Name`で指定したモジュールのみを再コンパイルすることができます。

以下実行例:

```elixir
# hoge.ex
defmodule Hoge do
  def hoge, do: IO.puts "hogeeee"
end
```

この`hoge.ex`をロードする。

```elixir
iex(3)> c("hoge.ex")
[Hoge]
iex(4)> Hoge.hoge
hogeeee
:ok
```

その後、`hoge.ex`を以下のように編集。

```elixir
defmodule Hoge do
  def hoge, do: IO.puts "fugaaaaaa"
end
```

これを`r`で再読み込み。

```elixir
iex(6)> r Hoge
warning: redefining module Hoge (current version defined in memory)
  hoge.ex:1

{:reloaded, Hoge, [Hoge]}
iex(7)> Hoge.hoge
fugaaaaaa
:ok
```

なお、`iex -S mix`で起動している場合は、`recompile`でmixのプロジェクト全体を再コンパイルできます。

