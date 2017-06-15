---
layout: post
title: ブログをGithub Pagesに移行して溜まった知見
tags:
- Others
- Jekyll
---

ブログをGithub Pagesに移行してしばらく経ったので、溜まった知見を共有しておきます。

## 記事一覧ページ
記事一覧自体は`site.pages`とか`site.tags`とかを使えば簡単に作れますが、1ページに表示する記事数を制限したり、タグごとの記事一覧を作る機能はありません。

なので、このサイトの[記事一覧](/articles.html)や[カテゴリごとの記事一覧](/articles.html#Haskell)では、一度全記事を表示してからJavaScriptで必要な範囲以外を非表示にするという強引な手段を取っています。

## テーマ
[Githubが公式に提供しているやつ]https://pages.github.com/themes/)をそのまま引っ張ってきて、魔改造して使っています。

## コメント欄
[Disqus](https://disqus.com/)というサイトを使うと簡単にコメント欄を設置できます。

## 下書き
リポジトリ直下に`_draft`というディレクトリを作ってそこに放り込んでいます。Jekyllはアンダースコアで始まるパスを無視してくれるので、これで外部から見ることはできません。記事が完成し次第`_posts/`に突っ込んでいます。

## 数式
[Mathjax](https://www.mathjax.org/)を使っています。ただし、ローカルに置くとファイル数が増えてJekyllがアホみたいに重くなるので、CDNを使うのがいいかと思います。

また、必要ないページで無駄にMathJaxみたいなでかいものを読み込ませるのも嫌なので、ページのヘッダに

```
---
...
use_mathjax: true
---
```

というように指定しておいて、MathJaxを読み込む側では

``` html
{% if page.use_mathjax %}
<script type="text/javascript" async
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_CHTML"></script>
{% endif %}
```

というようにして必要のないときに読み込まれないようにしています。

### 絵文字
Font Awesome<i class="fa fa-bug" aria-hidden="true"></i>を使っています。これも上述した理由により[CDN](https://www.bootstrapcdn.com/fontawesome/)を使ったほうがいいでしょう。

使える絵文字の一覧は以下のサイトを見ると楽です。
[http://fontawesome.io/icons/](http://fontawesome.io/icons/)

### 絵文字(2)
`_config.yml`に以下のように設定を加えることで、`:hoge:`形式の絵文字を使うことができるようになります。

```
gems:
  - jemoji
```

使える絵文字の一覧は以下のサイトを見ると楽です :joy: :joy: :joy: :joy: :joy: :joy:
[https://www.webpagefx.com/tools/emoji-cheat-sheet/](https://www.webpagefx.com/tools/emoji-cheat-sheet/)

## はてなスター
実は外部サイトでもはてなスターは使えます。

[はてなスターをブログに設置するには - はてなスター日記](http://d.hatena.ne.jp/hatenastar/20070707)

## ファイル分割
割りと細かめにファイル分けをしていて、なるべく一つのファイル内に必要なCSSとかJSも全部書き込んでいます。使いたい側からは

```
{% include foo.html hoge=fuga bar=baz %}
```

というようにして呼び出しています。Web Componentsの劣化版みたいな感じです。

このように分割することによってかなり管理が楽になります。例えば、記事一覧ページと個別記事ページの記事表示部分は全く同じテンプレートを使っているので、一つのファイルを弄るだけで両方に変更を反映することができます。

ただ、こうすると複数回同じテンプレートを呼び出したときに同じ`<style>`や`<script>`が何度も展開されてしまうので、そういった場合のみCSSやJavaScriptを外部に分けるようにしています。