---
layout: post
title: 手作りパーセプトロンで空間上の点を分類する
tags:
- OCaml
- 機械学習
use_mathjax: true
---

一年ほど前にGaucheでパーセプトロンを書いた記憶があるのですが、その時はまだ理論的な部分についてちゃんと理解していなかったので、もう一度やり直してみました。

以下では、観測データを \\( x \\) とし、その分類を \\( y \\) または \\( h(x) \\) と書くことにします。また、今回は二値分類なので、常に \\( y = 1, -1 \\) のいずれかとします。

オンライン学習は観測データを1つ読み込むごとに分類機 \\( h \\) を更新していくタイプの学習方法です。一つのデータを読み込んで学習し、分類器を更新することをラウンドといい、すべてのデータに対して一通りラウンドを行うことをエポックといいます。

具体的には、オンライン学習は以下のようなコードで表すことができます。

``` ocaml
open Lacaml.D

module type ONLINE_LEARNING =
  sig
    (* 分類器の型 *)
    type hyp

    (* 分類器を使って分類を行う *)
    val evaluate : hyp -> vec -> float

    (* オンライン学習のラウンド。分類器を使って分類を行い、分類器を破壊的に更新する。 *)
    val round : hyp -> vec -> float -> unit

    (* 分類器の初期状態 *)
    val initial_hyp : unit -> hyp
  end
```


パーセプトロンはオンライン学習のうちの一つである、勾配降下法というアルゴリズムの一種です。分類器を観測データと同じ次元のベクトル \\( w \\) で表し、平面 \\( \langle w, x \rangle \\) の上下のどちらに観測データがあるかで分類を行います。すなわち、

\\[ h(x) = \mathrm{sig}(\langle w, x \rangle) \\]

となります。

この際、実際の分類と分類器による結果の間にずれが生じた場合、そのずれを表す関数(=損失関数)の極小値に向かって少しずつ \\( w \\) を降下させていくイメージです。(あくまでイメージです。実際はもう少し複雑なので、詳しく知りたい人は本を買って読んで下さい)

具体的には、t回目のラウンドでの分類器が \\( w_t \\) で、その実際の分類が \\( y \\) であった場合、このラウンドでの観測データ \\( x_t \\) について、 \\( \langle w_t, x_t \rangle \\) と \\( y_t \\) との符号が一致すれば \\( w_t \\) は更新せず、もし符号が一致しなかった場合は、定数 \\( \eta \\) を用いて、

\\[ w_{t+1} = w_t + \eta y_t x_t \\]

という式で \\( w \\) を更新します。

``` ocaml
module type PERCEPTRON_PARAMS =
  sig
    (* 分類する空間の次元 *)
    val dim : int

    (* オンライン勾配降下法の正規化項の係数。大きいほどミスに対して鋭敏に反応するが、
     * 過学習の可能性が高まる(はず) *)
    val eta : float
  end

module Perceptron (P : PERCEPTRON_PARAMS) : sig
  include ONLINE_LEARNING
  val as_vec : hyp -> vec
  end =
  struct
    (* 分類器は dim + 1 次元のベクトルとする。 *)
    type hyp = vec

    let dim = P.dim
    let eta = P.eta

    (* バイアス項に相当する部分の係数 *)
    let bias (w : hyp) : float = w.{dim + 1}

    let sign (x : float) : float =
      if x = 0. then 0.
      else if x > 0. then 1.
      else -1.

    let evaluate (w: hyp) (x : vec) : float =
      dot ~n:dim w x +. bias w |> sign

    let round (w : hyp) (x : vec) (y : float) : unit =
      let y' = evaluate w x in
      if y' *. y <= 0.
      then
        let () =
          axpy ~alpha:(eta *. y) x w |> ignore;
          w.{dim + 1} <- w.{dim + 1} +. eta *. y in
        ()
      else ()

    let initial_hyp () = Vec.make0 (dim + 1)

    let as_vec (w : hyp) : vec = w
  end
```

`Lacaml`を使っているのでよくわからない関数が出てきますが、それ以外は特に難しい部分はないかと思います。

ちなみに、このコードでは`w`の次元を1つ上げて、

\\[ \langle w, x \rangle + w_0 \\]

の正負で分類を行っています。こうすることによって、原点を通らない平面も扱うことができます。

ちなみに、この方法は \\( x \\) の次元を一つ上げて、 \\( x = (x_1, x_2, ..., x_n, 1) \\) とすれば先ほどと変わらないため、理論的には違いはありません。

実際に使ってみます。

``` ocaml
module TestPerceptron = Perceptron (struct
                                     let dim = 2
                                     let eta = 1.
                                   end)
```

2次元の点を分離します。 `eta` は適当。

\\( [0, 100) \times [0, 100) \\) 上に適当に点をプロットしてみます。

``` ocaml
let rec random_point a b =
  let x = Random.float 100. in
  let y = Random.float 100. in
  if y > a *. x +. b then (Vec.of_list [x; y], 1.)
  else if y < a *. x +. b then (Vec.of_list [x; y], -1.)
  else random_point a b
```

`(x, y) = random_point a b`のとき、点`x`が直線`y = ax + b`よりも上にあれば`y`の値は`1`、そうでなければ`-1`となります。

とりあえず直線`y = 0.5x + 5`の上下に、点を一万個くらい打ってみます。

``` ocaml
let random_points = Array.init 10000 (fun _ -> random_point 0.5 5.)
```

分類器 \\( w \\) を初期化します。

``` ocaml
let w = TestPerceptron.initial_hyp ()
```

わかりやすいように、現在の`w`が表す直線の方程式を表示できるようにしてみましょう。

``` ocaml
let pp w =
  let v = TestPerceptron.as_vec w
  in let (a, r, b) = (v.{1}, v.{2}, v.{3}) in
  Printf.printf "y = %fx + %f\n" (-.a /. r) (-.b /. r)
```

先ほど打った点全体に対するエポックを定義します。

``` ocaml
let epoch () =
  let f (ok, ng) (x, y) =
    let y' = TestPerceptron.evaluate w x in
    let () = TestPerceptron.round w x y in
    if y' *. y <= 0.
    then (ok, ng + 1)
    else (ok + 1, ng) in
  Array.fold_left f (0, 0) random_points
```

上の関数は1エポック実行を行い、`w`の値を破壊的に変更し、正解した数と失敗した数をそれぞれ返す関数です。

それでは実行してみます。

``` ocaml
# let _ = epoch ();;
- : int * int = (9572, 428)
```

初回から割と成績がいいですが、それでも428個の分類ミスがあります。

ちなみに、この時の分離平面は、

``` ocaml
# pp w;;
y = 0.604140x + 0.186378
- : unit = ()
```

ということなので、1エポックでもとの方程式(`y = 0.5x + 5`)にそれなりに近づいていることがわかります。

エポックを回し続けてみます。

``` ocaml
# epoch ();;
- : int * int = (9665, 335)
# epoch ();;
- : int * int = (9677, 323)
# epoch ();;
- : int * int = (9676, 324)
# epoch ();;
- : int * int = (9711, 289)
# epoch ();;
- : int * int = (9718, 282)
# epoch ();;
- : int * int = (9714, 286)
# pp w;;
y = 0.612340x + 0.857867
- : unit = ()
```

少しずつ正解に近づいているのがわかります。

面倒なので100エポックずつ実行してみます。

``` ocaml
let rec times n action res =
  if n < 0 then failwith "n < 0!"
  else if n = 0 then res
  else let res' = action () in times (n - 1) action res'
```

これは100回`action`を実行して、最後の結果を返す関数です。

``` ocaml
# times 100 epoch (0, 0);;
- : int * int = (9899, 101)
```

100エポック回すと、いきなりミスの割合が1%近くまで減りました。

ちなみに、このときの分離平面は、

``` ocaml
# pp w;;
y = 0.508634x + 3.922920
- : unit = ()
```

となっているので、かなり正解に近づいていることがわかります。

もう少しやってみましょう。

``` ocaml
# times 100 epoch (0, 0);;
- : int * int = (9921, 79)
# times 100 epoch (0, 0);;
- : int * int = (9955, 45)
# times 100 epoch (0, 0);;
- : int * int = (9960, 40)
# times 100 epoch (0, 0);;
- : int * int = (9961, 39)
# pp w;;
y = 0.505604x + 4.718400
- : unit = ()
```
徐々に正解に近づいています。

``` ocaml
# times 100 epoch (0, 0);;
- : int * int = (9944, 56)
# times 100 epoch (0, 0);;
- : int * int = (9958, 42)
# times 100 epoch (0, 0);;
- : int * int = (9944, 56)
# times 100 epoch (0, 0);;
- : int * int = (9958, 42)
# times 100 epoch (0, 0);;
- : int * int = (9952, 48)
times 100 epoch (0, 0);;
- : int * int = (9974, 26)
# times 100 epoch (0, 0);;
- : int * int = (9977, 23)
# times 100 epoch (0, 0);;
- : int * int = (9971, 29)
# times 100 epoch (0, 0);;
- : int * int = (9979, 21)
# times 100 epoch (0, 0);;
- : int * int = (9963, 37)
```

面倒くさくなってきたので2000回ずつエポックを回していきます。

``` ocaml
# times 2000 epoch (0, 0);;
- : int * int = (9979, 21)
# times 2000 epoch (0, 0);;
- : int * int = (9982, 18)
# times 2000 epoch (0, 0);;
- : int * int = (9977, 23)
# times 2000 epoch (0, 0);;
- : int * int = (9981, 19)
# times 2000 epoch (0, 0);;
- : int * int = (9984, 16)
# times 10000 epoch (0, 0);;
- : int * int = (9984, 16)
```

ほとんど結果が変わらなくなってきたので、このへんにしておきたいと思います。

最終的な分離平面は

``` ocaml
# pp w;;
y = 0.501337x + 4.964476
- : unit = ()
```

となり、かなりの精度で正解に近づきました。
