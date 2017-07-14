---
layout: post
title: パーセプトロン万能説
tags:
- OCaml
- 機械学習
---

パーセプトロンのすごい所は、NAND回路を再現できるところにあるようです。

実際、NAND回路を再現するパーセプトロンは簡単に再現することができます。

```ocaml
open Lacaml.D
(* パーセプトロンは以下で定義したもの
 * https://genkami.github.io/2017/07/02/02-handmade-perceptron.html
 *)
open Perceptron

module P = Perceptron(struct
                       let dim = 2
                       let eta = 1.
                     end)

(* 入力は、0または1のいずれか *)
let xs = List.map Vec.of_list
                  [ [0.; 0.]
                  ; [0.; 1.]
                  ; [1.; 0.]
                  ; [1.; 1.]]

(* 出力は、1または-1の二値分類 *)
let ys = [1.; 1.; 1.; -1.]

let w = P.initial_hyp ()
```

これを適当に学習させます。

```ocaml
let () = List.iter2 (P.round w) xs ys
let ys' = List.map (P.evaluate w) xs
(* = [0.; -1.; -1.; -1.] *)
```

最初はこんな感じですが、数回エポックを回すと、

```ocaml
let () = List.iter2 (P.round w) xs ys
let ys' = List.map (P.evaluate w) xs
(* = [1.; 1.; 1.; -1.] *)
```

このように、見事にNANDを再現してくれていることがわかります。

当然ですが、NAND回路を適当に複数個繋いでいけば、フリップフロップなども含めた任意の論理回路を再現できてしまいます。

ということは、十分に大きな数のパーセプトロンを複雑に組み合わせたネットワークに対して、適切な学習をさせることができれば、任意の論理回路をそのネットワークで再現することができるようになります。

まあ、問題はその「適切な学習」をどのように行うかという部分なのですが。

その辺についてはまた今度。

その前に試験がやばい。
