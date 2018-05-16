---
layout: post
title: JQuery
tags:
- JavaScript
---

`.html()`, `.text()` で対象の要素が複数ある場合

http://api.jquery.com/html/

> If the selector expression matches more than one element, only the first match will have its HTML content returned. 

http://api.jquery.com/text/
> The result of the .text() method is a string containing the combined text of all matched elements.

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

