---
layout: post
title: "ocamloptの謎のエラー「wrong file naming」"
tags:
- OCaml
---

次のような2つのファイルを用意します。

``` ocaml
(* mymodule.ml *)
let hello () = print_endline "hello"
```

``` ocaml
(* hoge.ml *)
let () = MyModule.hello ()
```

これをビルドしようとすると、以下のようなエラーが出ます。

```
$ ocamlfind ocamlopt mymodule.ml hoge.ml
File "hoge.ml", line 1:
Error: Wrong file naming: myModule.cmi contains the compiled interface for
Mymodule when MyModule was expected
```

ぱっと見よくわからないエラーですが、どうやらこれは、「`MyModule`じゃなくて`Mymodule`が正しい名前だよ」ということを言ってくれているようです。

`hoge.ml`内で参照されている`Mymodule`の大文字小文字を正しく直せば問題なくビルドできるようになります。
