---
layout: post
title: Jekyll + Github Pages 入門
tags:
- Jekyll
---

この記事は[過去のブログ](http://monamonamonad.github.io/2017/03/14/jekyll-first.html)から移行したものです。

ちょっとしたtipsやメモ書き程度にブログ使うのもなんか違う気がするので、
Github Pagesに移行することにした。

### Jekyll の使い方

    $ cd monamonamonad.github.io
    $ jekyll s

ディレクトリ構成はこんな感じ

    /
      index.html
      config.yml
      _posts/
        YYYY-MM-DD-filename.md
        ...
      _layouts/
        default.html
        ...
      _includes/
        header.html
        footer.html
        ...

### Markdownのオプション

    ---
    layout: hoge
    title: this is a title
    tags:
    - hoge
    - fuga
    ---

MarkdownでもHTMLでもファイル先頭にメタ情報を書ける。

オプションでGithub Flavored Markdownが使えるらしい。必要になり次第調べるかも。
