---
layout: post
title: WordPressで独自のpost typeを定義する
tags:
- PHP
- WordPress
---

PHPもWordPressも全然知らないのにたまに触らないといけない状態が来てしまってつらい。

WordPressには投稿の種類を定義することができ、投稿の種類はpost type(投稿タイプ?)と呼ばれているようです。

post type (投稿タイプ?)

+ http://www.wpbeginner.com/wp-tutorials/how-to-create-custom-post-types-in-wordpress/
+ https://techacademy.jp/magazine/2842

1. function.php?をいじる
2. 管理画面のサイドバーに新しい投稿タイプに関するメニューが増えるので、そこから投稿を行う
3. 特定のpost typeのポストの取得方法は？

## お試し環境構築

```yaml
version: '3'
services:
  wordpress:
    image: wordpress
    ports:
      - "8080:80"
    environment:
      WORDPRESS_DB_PASSWORD: hogehoge

  mysql:
    image: mysql:5.7
    environment:
      MYSQL_ROOT_PASSWORD: hogehoge
```

このような`docker-compose.yml`を作った後、`docker-compose up`して`localhost:8080`から初期設定を適当に行えば、いい感じのWordPress環境が完成しているはずです。

## Post typeの定義

post typeの定義は、`functions.php`で行うのが一般的なようです。メニューの「外観」→「テーマ」→「テーマの編集」から、`functions.php`を選んで編集します。

```php

```
