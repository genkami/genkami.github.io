---
layout: post
title: '"alias Hoge.{A, B}"の"{A, B}"って何だ?'
tags:
- Elixir
---

Elixirでは普段何気なく`alias Hoge.{A, B}`みたいに書いていますが、この`alias`にしか出てこない`Hoge.{A, B}`という構文は文法的にどういう意味になっているのでしょうか?

`quote`して構文木を見てみると、`Hoge.{A, B}`は`Hoge.{}/2`という関数呼び出しに相当する構文らしいということがわかります。

{% raw %}
```elixir
iex(11)> quote do Mod.func(a, b) end
{{:., [], [{:__aliases__, [alias: false], [:Mod]}, :func]}, [],
 [{:a, [], Elixir}, {:b, [], Elixir}]}
iex(12)> quote do Mod.{a, b} end
{{:., [], [{:__aliases__, [alias: false], [:Mod]}, :{}]}, [],
 [{:a, [], Elixir}, {:b, [], Elixir}]}
```
{% endraw %}

ということは、`Hoge.{}/2`を定義することができれば、`Hoge.{A, B}`を意味のある式にすることができます。

以下のように、通常の`def`では`{}`という名前の関数を定義することはできません。

```elixir
iex(9)> defmodule Invalid do
...(9)>   def {}(a, b) do
...(9)>     a + b
...(9)>   end
...(9)> end
** (SyntaxError) iex:10: syntax error before: '('
```

しかし、マクロを使って無理やり構文木をねじ込めば、`{}`も定義できるのではないでしょうか?

そのために、まずは通常の関数定義がどのような構文木になるのかを調べてみます。

```elixir
iex(13)> quote do def func(a, b) do c end end
{:def, [context: Elixir, import: Kernel],
 [
   {:func, [context: Elixir], [{:a, [], Elixir}, {:b, [], Elixir}]},
   [do: {:c, [], Elixir}]
 ]}
```

これを参考にして、以下のようなマクロ`defbraces`を作ります。このマクロは、

```elixir
defbraces do
  def hoge1(x), do: ...
  def hoge2(x, y), do: ...
  ...
end
```

というように`defbraces`を呼び出した時に、その中にある全ての`def`の関数名を`{}`で書き換えた構文木を元の位置に挿入するマクロとなります。

このマクロの実装自体は、以下のように簡単に行うことができます。

```elixir
# defbraces.ex
defmodule DefBraces do
  defp replace_def_func_name(
    {:def, def_opts, [{_func_name, func_opts, args} | rest_def]},
    new_func_name
  ) do
    {:def, def_opts, [{new_func_name, func_opts, args} | rest_def]}
  end

  defmacro defbraces [do: do_block] do
    defs = case do_block do
             {:def, _, _} -> [do_block]
             {:__block__, _, defs} -> defs
           end
    defs
    |> Enum.map(&replace_def_func_name(&1, :{}))
  end
end
```

これを実際に使ったコードも書いてみます。

```elixir
# use_defbraces.ex
defmodule UseDefBraces do
  require DefBraces

  DefBraces.defbraces do
    def braces(a), do: a
    def braces(a, b), do: a + b
    def braces(a, b, c), do: a + b + c
  end
end
```

これを実行すると、実際に`{}/1`, `{}/2`, `{}/3`が定義されており、`UseDefBraces.{a, b}`のような形で呼び出せることが分かります。

```elixir
iex(4)> c("defbraces.exs")
[DefBraces]
iex(5)> c("use_defbraces.exs")
[UseDefBraces]
iex(6)> UseDefBraces.{3}
3
iex(7)> UseDefBraces.{3, 4}
7
iex(8)> UseDefBraces.{3, 4, 5}
12
```

このように、一応`Hoge.{A, B}`のような形の式に意味を持たせることはできるようです。まともな使い道があるかどうかは分かりませんが……。
