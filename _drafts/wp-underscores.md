---
layout: post
title: WordPressのアンダースコア系の関数__, _e, _x, _nとか
tags:
- PHP
- WordPress
---

コンテキストとは？の話

ドメインとは？の話

+ `__($text, $domain)`: ドメイン`$domain`でテキスト`$text`を翻訳する
+ `_x($text, $context, $domain)`: ドメイン`$domain`のコンテキスト`$context`でテキスト`$text`を翻訳する
+ `_n($single, $plural, $number, $domain)`: `$number`が1なら`$single`を、それ以外なら`$plural`をドメイン`$domain`で翻訳したものを返す
+ `_nx($single, $plural, $number, $context, $domain)`: `$number`が1なら`$single`を、それ以外なら`$plural`をドメイン`$domain`のコンテキスト`$context`で翻訳したものを返す
+ `_e($text, $domain)`: `echo __($text, $domain)`と同じ
+ `_ex($text, $context, $domain)`: `echo _x($text, $context, $domain)`と同じ
