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

と書くことができます。事前にコンパイルしてtagを1つのJSにまとめることもできるようですが、遊びで使うだけならこの方式で大丈夫でしょう。

## タグの書き方

基本的に1ファイルにHTML, CSS, JavaScriptを詰め込みます。

```html
<foobar>
  <p>{ hoge }</p>
  <style>
    p { color: #abcdef; }
  </style>
  this.hoge = piyo;
  foo () {
     ...
  }
</foobar>
```

JSは`<script>`タグの中に書くこともできます。メソッドの書き方からわかるように、素のJSというよりは、オブジェクトリテラルやクラスの中にいるような感覚です。

HTMLの中には`{ ... }`でJSの値を埋め込めます。JS側の`this.hoge`がHTMLからは`hoge`で呼ぶことができます。


## タグから値を受け取る

タグには`属性名=値`という形式で値を渡すことができます。

```html
<foobar baz={ someValue }></foobar>
```

渡す値はリテラルだけではなく、オブジェクトなども可能です。

タグに渡された属性は、`opts`から参照することができます。

```html
<hoge content="hello"></hoge>
...
<hoge>
  <p>{ opts.content }</p> <!-- 渡した "hello" が表示される -->
</hoge>
```

## 入れ子のタグ

独自のタグの中に他の要素を入れ子にすることができます。

```html
<foobar>
  <p>My name is foobar.
</foobar>
```

この場合、親で`<yield>`タグを使うと、その位置に子要素の内容がすべてレンダリングされます。

```html
<foobar>
  <h1>FooBar!!</h1>
  <yield></yield>
</foobar>
```


## riotの起動

```javascript
riot.compile(function() {
  var tags = riot.mount('*')
})
```

`riot.mount(hoge)`でhogeタグをマウントすることができます。

ちなみに、事前にコンパイル済みの場合は`riot.compile(...)`は不要です。

## イベント
イベントは、`this.on('EventName', () => { ... });`の形でハンドリングすることができます。

重要なイベントとして、最初にタグが生成される時に`mount`イベントが、表示が更新されるときは`update`イベントが発火されます。

```javascript
this.on('mount',() => { ... });
```

ちなみに、画面更新は基本的に`riot.update()`か`this.update()`を呼ばれたときにしか行われません。

また、`this.trigger('EventName', ...args)`で独自のイベントを発火させることもできます。

## observable
要素だけでなく、任意のオブジェクトに対してイベントの通知を行うことができます。

```javascript
riot.observable(hoge);
```

これで`hoge`が`on`と`trigger`を使えるようになります。また、引数無しで`riot.observable()`を実行すると、新しいobservableなオブジェクトが返されます。

この方法を使って親子間通信や兄弟間通信をおこなうことができます。

## 属性を渡すときの諸注意

+ `checked`等にはboolを渡せる
+ `class`は`{ name1: true, name2: false, ... }`のようにすることで、`true`になっている名前のクラスのみ付与することができる
+ `onHoge`はイベントハンドラとしての関数を渡せる

## 便利な属性
+ `if={contidion}`: `condition`が`true`のときだけ表示
+ `each={objs}`: `objs`の中身ごとに繰り返す。`objs`の要素がそれ以下のスコープにばらまかれる。前まで`this`だったやつには`parent`でアクセスできる

## その他

その他はだいたい[ここ](http://riotjs.com/ja/guide/)にあります
