---
layout: post
title: OCamlのラベル付き引数
tags:
- OCaml
---

ラベル付き引数とかオプション引数とか。すぐ書き方忘れるのでメモ。

仮引数の定義は以下のいずれか

```
~labal:pat (labeがpatと同じ場合、省略可)
?label:(pat:typ=default) (typは省略可。labelに関しては上と同様)
```

実引数の与え方は以下の通り

```
func ~labeled:value
```

実際に使ってみる:

``` ocaml
(* Jane Street 流 fold_left 😃 *)
let rec fold_left (xs : 'a list) ~(init:'b) ~(f:('b -> 'a -> 'b)) : 'b =
  match xs with
  | [] -> init
  | x :: xs' -> js_fold_left xs' ~init:(f init x) ~f:f

let iota ?(step : int = 1) ?(start : int = 0) (end_ : int) : int list =
  let rec iter start end_ result =
    if end_ < start
    then result
    else iter start (end_ - step) (end_ :: result)
  in iter start (end_ - step) []
```

実行例:

``` ocaml
# fold_left [2; 4; 6; 8; 10] ~f:(+) ~init:0;;
- : int = 30
# iota 10;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
# iota ~start:5 10;;
- : int list = [5; 6; 7; 8; 9]
# iota ~start:2 ~step:2 10;;
- : int list = [2; 4; 6; 8]
```

