---
layout: post
title: Ectoを初めて使ってみたので基本的な書き方とかメモ
tags:
- Elixir
---

Ectoの使い方がなんとなく分かってきたので、Ectoを使ってデータベースの操作を行っていくための大まかな流れをメモしておきます。それぞれの機能の詳細には触れないので、そのあたりは公式のドキュメントなりを読むといいと思います。

## 最初の設定
新しいアプリケーション`ecto_example`を作ります。

```sh
$ mix new ecto_example
```

生成されたファイルを、以下のように書き換えていきます。

```elixir
# mix.exs
...
  def application do
    [
      extra_applications: [:logger],
      mod: {EctoExample.Application, []}
    ]
  end
...
  defp deps do
    [
      {:ecto, "~> 2.2"},
      {:postgrex, "~> 0.13"}  # PostgreSQLを使う場合
    ]
...
```

```elixir
# config.exs
...
# リポジトリ(後述)の指定
config :ecto_example, :ecto_repos, [EctoExample.Repo]
...
```

```elixir
# lib/ecto_example/application.ex
defmodule EctoExample.Application do
  use Application

  def start(_type, _args) do
    children = [
      EctoExample.Repo # リポジトリ(後述)
    ]

    opts = [strategy: :one_for_one, name: EctoExample.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

## リポジトリの作成

リポジトリとは、バックエンドで動くデータベースを指定するものです。

手作業でもリポジトリを作成することはできますが、テンプレートを生成してくれる機能があるのでこれを使ったほうが楽です。

```sh
$ mix deps.get
$ mix ecto.gen.repo
* creating lib/ecto_example
* creating lib/ecto_example/repo.ex
* updating config/config.exs
```

コマンドを実行すると、以下のようにファイルが追加/変更されます。

```elixir
config :ecto_example, EctoExample.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "ecto_example_repo",
  username: "user",
  password: "pass",
  hostname: "localhost"
```

```elixir
# lib/ecto_example/repo.ex
defmodule EctoExample.Repo do
  use Ecto.Repo, otp_app: :ecto_example
end
```

`config.exs`は接続するデータベースの指定なので、`username`や`password`等は必要があれば適宜書き換えます。`repo.ex`は基本的にそのままで十分です。

## テーブルの作成

マイグレーション用のコードのテンプレを生成して、それを弄っていくのが基本的な流れになります。

```sh
$ mix ecto.gen.migration user
Compiling 2 files (.ex)
Generated ecto_example app
* creating priv/repo/migrations
* creating priv/repo/migrations/20180308191129_user.exs
```

コマンドの引数に含まれる`user`はただのファイル名の指定なので、そこまで大きな意味はありません。

このコマンドを入力することによって、マイグレーション用のコードのテンプレが、`(日付)_user.exs`という名前で生成されました。この内容は以下のようになっています。

```elixir
# priv/repo/migrations/日付_user.exs
defmodule EctoExample.Repo.Migrations.User do
  use Ecto.Migration

  def change do

  end
end
```

これを変更して、とりあえず最低限のフィールドを追加します。

```elixir
defmodule EctoExample.Repo.Migrations.User do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string, unique: true
    end
  end
end
```

これを保存した状態で、以下のようにマイグレーションを行います。

```sh
$ mix ecto.migrate
```

マイグレーションが完了すると、テーブルが定義されていることが分かります。実際に`psql`で確認してみましょう。

```psql
ecto_example=# \dt;
             List of relations
 Schema |       Name        | Type  | Owner
--------+-------------------+-------+-------
 public | schema_migrations | table | admin
 public | users             | table | admin
(2 rows)


ecto_example=# select * from users;
 id | name
----+------
(0 rows)

```

migration用の履歴っぽい`schema_migrations`と、先ほど定義した`users`の2つのテーブルが定義されていることがわかります。

また、`id`フィールドは特に指定しなくても自動生成されます。この機能は`create table("table_nam", primary_key: false)`という引数を指定することによってオフにすることができます。

## テーブルの変更

先ほど定義した`users`テーブルに、他のフィールドを追加してみます。

テーブルの変更も先ほどと同様に、マイグレーション用のコードのテンプレを生成して弄っていく方向でやっていきます。

```sh
$ mix ecto.gen.migration user
* creating priv/repo/migrations
* creating priv/repo/migrations/20180309174458_user.exs
```

生成された`(新しい日付)_user.exs`を以下のように改変して、フィールドの追加と変更を行っていきます。

```elixir
# /priv/repo/migrations/(新しい日付)_user.exs
defmodule EctoExample.Repo.Migrations.User do
  use Ecto.Migration

  def change do
    alter table(:users) do
      modify :name, :string, null: false, unique: true
      add :age, :integer, null: false
      add :address, :string, null: true
    end
  end
end
```

先ほどは説明しませんでしたが、`change/0`を定義すると、それを元にテーブルのアップグレード/ダウングレード用を行うためのSQL文を発行する`up/0`, `down/0`という関数が自動的に定義されます。

これを使ってテーブルを更新するには、もう一度`mix ecto.migrate`をすればOKです。

```sh
$ mix ecto.migrate
```

psqlでテーブルを確認すると、実際に変更されていることがわかります。

```psql
ecto_example=# select * from users;
 id | name | age | address
----+------+-----+---------
(0 rows)
```

## スキーマの定義
スキーマとは、テーブルとElixirのデータの間の変換をするものとなります。

```elixir
# lib/ecto_example/user.ex
defmodule EctoExample.User do
  use Ecto.Schema

  schema "users" do
    field :name, :string, null: false, unique: true
    field :age, :integer, null: false
    field :address, :string, null: true
  end
end
```

このような定義を行うと、`%EctoExample.User`構造体とその他諸々が生成されます。

## クエリの実行
テーブルの行の操作は、先ほどのリポジトリを介して行います。各行を表すデータは、スキーマにより定義される構造体の形で扱われます。

特定のフィールドの値に基づくデータの取得は、`Repo.get`等の関数を用います。

```elixir
iex(1)> alias EctoExample.{User, Repo}
iex(2)> # inserted には挿入後のデータが入る
iex(2)> {:ok, inserted} = Repo.insert %User{name: "Taro", age: 20, address: "Tokyo"}

03:21:08.518 [debug] QUERY OK db=4.2ms
INSERT INTO "users" ("address","age","name") VALUES ($1,$2,$3) RETURNING "id" ["Tokyo", 20, "Taro"]
{:ok,
 %EctoExample.User{
   __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
   address: "Tokyo",
   age: 20,
   id: 5,
   name: "Taro"
}}
iex(4)> Repo.get(User, inserted.id) # idを用いて取得

03:23:30.318 [debug] QUERY OK source="users" db=2.7ms
SELECT u0."id", u0."name", u0."age", u0."address" FROM "users" AS u0 WHERE (u0."id" = $1) [5]
%EctoExample.User{
  __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
  address: "Tokyo",
  age: 20,
  id: 5,
  name: "Taro"
}
```

複雑なクエリは、`Ecto.Query.from`を使って構築します。

```elixir
iex(2)> # 適当にユーザーを追加
iex(3)> {:ok, _} = Repo.insert %User{name: "Jiro", age: 15, address: "Tokyo"}
iex(4)> {:ok, _} = Repo.insert %User{name: "Saburo", age: 12, address: "Hokkaido"}

iex(5)> import Ecto.Query, only: [from: 2]
iex(9)> query = from u in User, where: u.age >= 15
iex(10)> Repo.all(query)

03:29:29.539 [debug] QUERY OK source="users" db=14.5ms decode=0.1ms
SELECT u0."id", u0."name", u0."age", u0."address" FROM "users" AS u0 WHERE (u0."age" >= 15) []
[
  %EctoExample.User{
    __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
    address: "Tokyo",
    age: 20,
    id: 5,
    name: "Taro"
  },
  %EctoExample.User{
    __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
    address: "Tokyo",
    age: 15,
    id: 6,
    name: "Jiro"
  }
]
```

## Changeset
chengesetとは、データのフィルタリングやバリデーション、差分管理などを行うための機能です。例えば以下のchangesetは、`users`テーブルに対して、必須のフィールドのチェックと`name`フィールドの長さのチェックを行っています。

```elixir
# lib/ecto_example/user.ex
defmodule EctoExample.User do
  ...
  import Ecto.Changeset
  @required_fields ~w(name age)a
  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
    |> validate_length(:name, min: 3, max: 32)
  end
end
```

これを使うと、与えられたデータが正しいものなのかどうかの検証を行うことができます。

```elixir
iex(20)> # 正しいデータ
iex(20)> changeset = User.changeset(%User{}, %{name: "John", age: 35, address: "US"})
#Ecto.Changeset<
  action: nil,
  changes: %{age: 35, name: "John"},
  errors: [],
  data: #EctoExample.User<>,
  valid?: true
>
iex(21)> # name は3文字以上
iex(21)> changeset = User.changeset(%User{}, %{name: "A", age: 35, address: "US"})
#Ecto.Changeset<
  action: nil,
  changes: %{age: 35, name: "A"},
  errors: [
    name: {"should be at least %{count} character(s)",
     [count: 3, validation: :length, min: 3]}
  ],
  data: #EctoExample.User<>,
  valid?: false
>
iex(22)> # age は必須
iex(22)> changeset = User.changeset(%User{}, %{name: "John Doe", address: "US"})
#Ecto.Changeset<
  action: nil,
  changes: %{name: "John Doe"},
  errors: [age: {"can't be blank", [validation: :required]}],
  data: #EctoExample.User<>,
  valid?: false
>
```

また、changesetは既存のデータの更新にも使えます。


```elixir
iex(27)> user = Repo.get_by(User, name: "Taro")

04:12:39.951 [debug] QUERY OK source="users" db=5.1ms
SELECT u0."id", u0."name", u0."age", u0."address" FROM "users" AS u0 WHERE (u0."name" = $1) ["Taro"]
%EctoExample.User{
  __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
  address: "Tokyo",
  age: 20,
  id: 5,
  name: "Taro"
}
iex(28)> changeset = User.changeset(user, %{age: user.age + 1})
#Ecto.Changeset<
  action: nil,
  changes: %{age: 21},
  errors: [],
  data: #EctoExample.User<>,
  valid?: true
>
```

このようにして得たchangesetを`Repo.insert`や`Repo.update`などの引数に与えることで、実際にクエリが発行されます。

```elixir
iex(29)> Repo.update(changeset) # changeset はさっきのやつ

04:13:51.126 [debug] QUERY OK db=11.7ms
UPDATE "users" SET "age" = $1 WHERE "id" = $2 [21, 5]
{:ok,
 %EctoExample.User{
   __meta__: #Ecto.Schema.Metadata<:loaded, "users">,
   address: "Tokyo",
   age: 21,
   id: 5,
   name: "Taro"
 }}
```

## その他
詳しいことは公式のドキュメントを読めばいいと思います。

[Ecto – Ecto v2.2.9](https://hexdocs.pm/ecto/Ecto.html)

## PS
新居にベッドが欲しいです

![/img/post/2018-03-10-polca.jpg](/img/post/2018-03-10-polca.jpg)
