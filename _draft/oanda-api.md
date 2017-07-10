---
layout: post
title: OANDAのAPIを使ってみる
tags:
- FX
- API
- OCaml
---

FX関連のAPIの中ではOANDAのものが一番使いやすそうだったので、試しに遊んでみることにしました。

公式のドキュメントはこちら↓

[開発ガイド | OANDA API](http://developer.oanda.com/docs/jp/v1/guide/)

基本的にこれを読むだけで大体のことは事足りそうです。

試しに銘柄一覧を取得してみました。

``` ocaml
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

let base_url = "http://api-sandbox.oanda.com/v1"

let instruments () : Yojson.Basic.json option Lwt.t =
  Client.get (Uri.of_string (base_url ^ "/v1/instruments")) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body >>= fun (strbody) ->
  try return (Some (Yojson.Basic.from_string strbody)) with
  | _ -> return None

let () =
  match Lwt_main.run (instruments ()) with
  | None -> print_endline "parse error"
  | Some json ->
     [json]
     |> filter_member "instruments"
     |> flatten
     |> filter_member "instrument"
     |> filter_string
     |> List.iter print_endline
```

ちなみに、sandbox以外のAPIはヘッダに`Authorization: Bearer MY-TOKEN`を追加すれば認証できるようです。
