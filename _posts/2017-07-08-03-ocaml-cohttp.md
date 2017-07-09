---
layout: post
title: OCamlでHTTPクライアント
tags:
- OCaml
---

cohttp-lwtというライブラリを使って、モナディックに非同期なHTTP通信を行うとよいらしいです。

## インストール

```
opam install tls cohttp # tlsを入れないとhttpsが使えない
```


## とりあえず実行

モナディックな記法にさえ慣れていれば、割りとスッキリ書けます。某言語みたいにHTTPリクエスト送りたいだけなのに謎演算子が大量に発生したりしないのも◎。

``` ocaml
(* httptest.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

let show_body url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body >>= fun (strbody) ->
  print_endline strbody;
  return ()

let () = Lwt_main.run (show_body "http://dokodeglobal.com/")
```

実行結果

```
$ ocamlfind ocamlopt -o httptest.native -linkpkg -package cohttp,lwt,cohttp.lwt httptest.ml
admin@porkadot [~/gomicode/OCaml] 2017/07/08 21:27:02
$ ./httptest.native
<!doctype html>
<html lang="ja">
  <head>
    <meta charset="utf-8">
    <title>dokode global</title>
    <link rel="stylesheet" href="css/style.css">
  </head>
  <body>
  ...
```

## POST

GETのときに`Client.get`を使ったように、POSTの場合は`Client.post`を使えます。その他のメソッドも一緒。

``` ocaml
let post =
  let reqbody = Uri.encoded_of_query [("hoge", ["fuga"]); ("foo", ["bar"])]
                |> Cohttp_lwt_body.of_string in
  Client.post ~body:reqbody (Uri.of_string "http://example.com/")
```

`post_form`を使うと、もう少し楽に書けます。

``` ocaml
let post =
  Client.post_form (Uri.of_string "http://example.com/")
                   ~params:[("hoge", ["fuga"]); ("foo", ["bar"])]
```

## ヘッダをつける

`~headers`引数にヘッダを設定することで、ヘッダの値も指定できます。

``` ocaml
let post =
  Client.post_form (Uri.of_string "http://example.com/")
                   ~params:[("hoge", ["fuga"]); ("foo", ["bar"])]
                   ~headers:Header.of_list [("User-Agent", "Mozilla/5.0")]

```

