---
layout: post
title: 『プログラミングElixir』日本語版以降のElixirの進化
tags:
- Elixir
---

『プログラミングElixir』の日本語版はElixir 1.2を想定して書かれていますが，現在の最新版は1.6.0-rc.1となり，そこそこバージョンが上がってしまっています．

そこで，これらのバージョン間での違いを雑にまとめました．

なお，書いている人はElixir初心者なので，説明不足や表記の間違いなどあればコメントなり[プルリク](https://github.com/genkami/genkami.github.io)なりでご報告ください．

## 文法の変更

### 変数名とatomのUnicode対応 (since 1.5)
(引用符なしの)atomと変数名に，Unicodeが使えるようになりました．例えば

```elixir
ほげ = :ふが
```

も1.5以降では有効な文になります．

詳細: [elixir/CHANGELOG.md at v1.5 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md)

### ネストしたブロック内での代入の非推奨化 (since 1.3)
`if`や`case`などの中で外側のスコープにある変数への代入を行うことが非推奨化されました．

詳細: [elixir/CHANGELOG.md at v1.3 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md)

## モジュール/関数の追加や仕様変更

### DynamicSupervisor (since 1.6)
`Supervisor`のstrategyの1つである`:simple_one_for_one`がdeprecatedになり，代わりに同等の機能を果たす`DynamicSupervisor`が追加されました．

詳細: [Elixir 1.6 で入る DynamicSupervisor について - Qiita](https://qiita.com/melpon/items/853a5bc2ff4279a5dfea)

### defguard, defguardp (since 1.6)
`defguard`と`defgaurdp`を使うことで，パターンマッチのガード節に書くことのできる条件式を自分で定義することができます．ただし，ガード節に任意の式が使えるようになったわけではなく，あくまで特定の条件に対するエイリアスのようなものを定義できるだけなので注意が必要です．

詳細: [Kernel – Elixir v1.7.0-dev](https://hexdocs.pm/elixir/master/Kernel.html#defguard/1)

### Supervisor.start_link/2 の引数 (since 1.5)
`Supervisor.start_link/2`が，`Supervisor.Spec.spec()`だけではなく，`module()`もしくは`{module(), term()}`の形の引数も受け取れるようになりました．

詳細 [Supervisor – Elixir v1.5.3](https://hexdocs.pm/elixir/Supervisor.html#start_link/2)

### Registry (since 1.4)
`Registry`という単純な分散型KVSのようなものが追加されました．

詳細: [Registry – Elixir v1.7.0-dev](https://hexdocs.pm/elixir/master/Registry.html)

### Task.async_stream (since 1.4)
並行版mapである`Task.async_stream/3`, `Task.async_stream/5`が追加されました．同時並行数なども指定できて便利です．

詳細: [Task – Elixir v1.5.3](https://hexdocs.pm/elixir/Task.html#async_stream/3)

### 時刻に関するデータ型などの追加 (since 1.3)
時刻に関するモジュールとデータ型`Calendar`, `Date`, `Time`, `NaiveDateTime`, `DateTime`と，時刻を扱うシジル`~D`, `~T`, `~N`が追加されました．

詳細: [elixir/CHANGELOG.md at v1.3 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md)

### get_inとかで使えるアクセサの追加 (since 1.3)
`Access.all/0`などを使うことで，~~JQueryライクに~~簡単にネストしたデータ構造の一部を取得したり書き換えたりできるようになりました．

詳細: [Access – Elixir v1.5.3](https://hexdocs.pm/elixir/Access.html#all/0)

### ExUnitに独自のテストの種類を追加できるようになった (since 1.3)
`ExTest.Case.register_test`を直接呼ぶことで，testやdoctestのようなテストの種類を独自で定義できるようです．

詳細: [ExUnit.Case – ExUnit v1.5.3](https://hexdocs.pm/ex_unit/ExUnit.Case.html#register_test/4)

### ExUnit.describe/2 (since 1.3)
ユニットテスト系のライブラリでよくある`describe`が`ExUnit`にも追加されました．使い方も想像通り．

詳細: [ExUnit.Case – ExUnit v1.5.3](https://hexdocs.pm/ex_unit/ExUnit.Case.html#describe/2)

### Named Setup (since 1.3)
`ExUnit.setup/1`, `ExUnit.setup_all/1`に，初期化時に実行してほしい関数の一覧をアトムで渡すことができるようになりました．

詳細: [elixir/CHANGELOG.md at v1.3 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md#named-setups-and-describes)

## 属性の追加

### @deprecated (since 1.6)
APIが非推奨であることを表す`@deprecated`という属性が追加されました．

詳細: [elixir/CHANGELOG.md at v1.6 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md)

### @since (since 1.6)
モジュールのAPIが追加されたバージョンを表す`@since`という属性が追加されました．

詳細: [elixir/CHANGELOG.md at v1.6 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md)

### @impl (since 1.5)
`behaviour`の`callback`の具体的な実装であることを明示するための属性として，`@impl`が使えるようになりました．

具体的な使い方としては，`@impl true`もしくは`@impl BehaviourName`で後続する関数定義が`callback`の実装であることを明示することができます．

詳細: [elixir/CHANGELOG.md at v1.5 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md)

## iex, mixの機能拡張

### formatter (since 1.6)
`mix format`でコードを整形してくれるようになりました．

詳細: [format – Mix v1.7.0-dev](https://hexdocs.pm/mix/master/Mix.Tasks.Format.html)

### mix xref (since 1.3)
`mix xref MODE`でモジュールの依存関係を解析したりしてくれるようになりました．

詳細: [xref – Mix v1.7.0-dev](https://hexdocs.pm/mix/master/Mix.Tasks.Xref.html)

### デバッグ関連の関数 (since 1.5)
`break!/4`, `continue/0`など，iex上でのデバッグに便利なヘルパー関数が追加されました．

詳細: [IEx.Helpers – IEx v1.5.3](https://hexdocs.pm/iex/IEx.Helpers.html#break!/2)

### applications: の省略 (since 1.4)
`mix.exs`の`application`が返す`applications:`のパラメータを省略できるようになりました．`deps`の依存関係から，必要なものを自動的に推論してくれるようです．

詳細: [Application inference in Elixir 1.4 – sergiotapia](https://sergiotapia.me/application-inference-in-elixir-1-4-ae9e43e90301)

### mix escript.install
escriptを直接インストールできるようになりました．`~/.mix/escripts`に`PATH`を通せば，一瞬でescriptを実行できるようになります．

詳細: [escript.install – Mix v1.7.0-dev](https://hexdocs.pm/mix/master/Mix.Tasks.Escript.Install.html)

### Git, Hexから直接インストール
`mix archive.install`, `mix escript.install`で直接hex, git, githubからインストールすることができるようになりました．具体的には以下のようにコマンドを入力することで，これらを行うことができます．

```sh
$ mix archive.install git https://gitrepo.example.com/path/to/repo
$ mix archive.install github user/project
$ mix archive.install hex package_name
```

詳細: [archive.install – Mix v1.5.3](https://hexdocs.pm/mix/Mix.Tasks.Archive.Install.html)

### mix app.tree, mix deps.tree
現在のプロジェクト内のアプリケーション一覧と，依存関係にあるプロジェクトも含めたアプリケーション一覧を取得するコマンド`mix app.tree`, `mix deps.tree`が追加されました．

詳細: [elixir/CHANGELOG.md at v1.3 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md)

### mix test --stale
`mix test --stale`を実行すると，最後に`mix test --stale`が実行された後に変更されたモジュールと，そのモジュールに依存するモジュールのみのテストが行われるようになります．

詳細: [elixir/CHANGELOG.md at v1.3 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md)

## 参考
+ [elixir/CHANGELOG.md at v1.6 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md)
+ [elixir/CHANGELOG.md at v1.5 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md)
+ [elixir/CHANGELOG.md at v1.4 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md)
+ [elixir/CHANGELOG.md at v1.3 · elixir-lang/elixir](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md)

