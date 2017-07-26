---
layout: post
title: 自分用Riot.js機能まとめ
tags:
- JavaScript
---

フロントエンドのフレームワークはたまにしか使わないのでよく詳細を忘れます。

## タグの置き場所

```html
<script type="riot/tag" src="./foobar.tag"></script>
```

と書くか、元のHTMLに直接

```html
<script type="riot/tag">
  <foobar>
    ...
  </foobar>
</script>
```

と書ける

## タグの書き方

基本的に1ファイルにHTML, CSS, JavaScriptを詰め込む

```html
<foobar>
  <p>hogehoge</p>
  <style>
    p { color: #abcdef; }
  </style>
  this.hoge = piyo;
  foo () {
     ...
  }
</foobar>
```

JSは`<script>`タグの中に書いても良い。素のJSではなく、オブジェクトリテラルの中にいるような感覚

HTMLの中には`{ ... }`でJSの値を埋め込める。JS側の`this.hoge`がHTMLからは`hoge`で呼べる。


## タグから値を受け取る

値も渡せる

```html
<foobar baz="piyo"></foobar>
```
渡す値はリテラルだけじゃなくてオブジェクトなども可

タグに渡された属性は、`opts`から参照できる

```html
<hoge>
  <p>{ opts.content }</p>
</hoge>
...
<hoge content="hello"></hoge>
```

## 入れ子のタグ


```html
<foobar>
  <p>My name is foobar.
</foobar>
```

親で`<yield>`タグを使うと、その位置に子要素の内容がすべてレンダリングされる


## riotの呼び出し

```javascript
riot.compile(function() {
  var tags = riot.mount('*')
})
```

これはtagを直接使ってるときの場合

事前にコンパイル済みの場合は`riot.compile(...)`はいらない

けっこう速いらしいのでそんなに気にせずクライアントサイドでコンパイルしてる

## イベント
最初にタグが生成される時に`mount`イベント

更新されるときは`update`イベントが発火される。

```javascript
this.on('mount',() => { ... });
```

ちなみに、画面更新は明示的に`riot.update()`か`this.update()`を呼ばれたときにしか行われない。

## 属性を渡すときの諸注意

`checked`等にはboolを渡せる

`class`は`{ name1: true, name2: false, ... }`のようにすることで、`true`になっている名前のクラスのみ付与することができる

`onHoge`はイベントハンドラとしての関数を渡せる

## 便利な属性
`if={contidion}`: `condition`が`true`のときだけ表示
`each={objs}`: `objs`の中身ごとに繰り返す。`objs`の要素がそれ以下のスコープにばらまかれる。前まで`this`だったやつには`parent`でアクセスできる

## その他

その他はだいたい[ここ](http://riotjs.com/ja/guide/)にあります

これにかんけいするやつあとで別記事でかく
http://qiita.com/iwamatsu0430/items/49ea5ad14c631f323d22
