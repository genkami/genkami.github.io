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