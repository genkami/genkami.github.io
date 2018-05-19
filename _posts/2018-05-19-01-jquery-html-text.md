---
layout: post
title: JQueryのhtml()とtext()の微妙な差にハマった話
tags:
- JavaScript
---

jQueryの `.html()`, `.text()` はそれぞれ指定した要素の中身をHTML/テキストで取得する関数です。この両者は似たような用途で使われがちですが、取得する内容以外にも微妙な挙動の違いがあるため注意が必要です。

具体的には、 `$(...)` で指定した対象の要素が複数ある場合が両者で大きく異なります。

`.html()` については、対象の要素が複数ある場合、その先頭要素の中身のみを返します。

[http://api.jquery.com/html/](http://api.jquery.com/html/)

> If the selector expression matches more than one element, only the first match will have its HTML content returned.

一方、 `.text()` については、対象の要素が複数ある場合、その全ての中身を連結したものを返します。

[http://api.jquery.com/text/](http://api.jquery.com/text/)

> The result of the .text() method is a string containing the combined text of all matched elements.


実際に、以下のようなHTMLの `$(".test")` に対して `.html()` と `.text()` を読んでみると、結果は以下のようになります。

``` html
<!doctype html>
<html lang="ja">
  <head>
    <meta charset="utf-8">
    <title>hoge</title>
  </head>
  <body>
    <span class="test">hogehoge</span>
    <span class="test">fugafuga</span>
    <script src="https://code.jquery.com/jquery-1.12.4.min.js"></script>
  </body>
</html>
```

``` javascript
> $(".test").html()
"hogehoge"
> $(".test").text()
"hogehogefugafuga"
```

