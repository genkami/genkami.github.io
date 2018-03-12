---
layout: post
title: WordPressで独自のpost typeを定義する
tags:
- PHP
- WordPress
---

PHPもWordPressも全然知らないのにたまに触らないといけない状態が来てしまってつらい。

WordPressの投稿は、「通常の投稿」「固定ページ」などのように複数の種類に分けられています。この投稿の種類のことを、post type(投稿タイプ?)と呼ぶようです。今回はこ
のpost typeを新たに登録する必要に迫られたので、行ったことのメモを残しておきます。

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
    volumes:
      - /path/to/my/wordpress:/var/www/html

  mysql:
    image: mysql:5.7
    environment:
      MYSQL_ROOT_PASSWORD: hogehoge
```

このような`docker-compose.yml`を作った後、`docker-compose up`して`localhost:8080`から初期設定を適当に行えば、いい感じのWordPress環境が完成しているはずです。

## Post typeの定義

post typeの定義は、`functions.php`で行うのが一般的なようです。`functions.php`は`wp-content/themes/(テーマ名)/`にあるので、これを開き、以下のようにpost typeの定義を行います。

```php
add_action('init', 'register_my_post_type');
function register_my_post_type() {
  register_post_type(
    'my-post-types',                        // 投稿タイプの識別子
    array(
      'labels' => array(
        'name' => __('my-posts'),           // 投稿タイプ名(複数形)
        'singular_name' => __('my-post')    // 投稿タイプ名(単数形)
      ),
      'description' => 'example post type', // 投稿タイプの説明
      'exclude_from_search' => false,       // 検索結果に表示するかどうか
      'public' => true
    )
  );
}
```

引数の詳細については、以下を参照:

+ [関数リファレンス/register post type - WordPress Codex 日本語版](https://wpdocs.osdn.jp/%E9%96%A2%E6%95%B0%E3%83%AA%E3%83%95%E3%82%A1%E3%83%AC%E3%83%B3%E3%82%B9/register_post_type)

`functions.php`の編集後WordPressの管理画面を開くと、先ほど追加したpost typeがサイドメニューに追加されていることがわかります。

![/img/post/2018-03-13-wp-side-menu.png](/img/post/2018-03-13-wp-side-menu.png)

独自のpost typeの記事の投稿や編集は、このサイドメニューから通常の投稿と同じように行うことができます。

![/img/post/2018-03-13-wp-edit-post.png](/img/post/2018-03-13-wp-edit-post.png)

## 特定のpost typeの記事のみを検索

通常の記事検索ページのクエリに`post_type=POST-TYPE-IDENTIFIER`を付けることで、指定したpost typeの記事のみを検索することができます。

![/img/post/2018-03-13-wp-search-post.png](/img/post/2018-03-13-wp-search-post.png)

