---
layout: post
title: Googleサイト内検索が表示されない
tags:
- JavaScript
---

このサイトの記事一覧ページだけサイト内検索が表示されない

gazou

```html
    <script>
     (function() {
       var cx = 'XXXXX';
       var gcse = document.createElement('script');
       gcse.type = 'text/javascript';
       gcse.async = true;
       gcse.src = 'https://cse.google.com/cse.js?cx=' + cx;
       var s = document.getElementsByTagName('script')[0];
       s.parentNode.insertBefore(gcse, s);
     })();
    </script>
    <gcse:searchbox-only></gcse:searchbox-only>
```

普通はこんな感じのHTMLを埋め込むはず

このfunction自体は呼ばれており、cse.jsがDOMに追加されていることは確認できた。

このcse.js内でエラー？

cse.jsを読んでみる

```javascript
var b=document.createElement("script"),
...
b.src=h+"";
b.type="text/javascript";
document.getElementsByTagName("head")[0].appendChild(b);
```

`cse.js`も別なスクリプトを読んでるだけっぽい(詳細は読む気が起きない)

headの中身を見てみたら、

https://www.google.com/uds/api/search/1.0/XXXXXXXX/default+ja.I.js

というのが読み込まれているよう。

これを気合で読む。

最終的には`<gcse:searchbox-only>`みたいなタグのあった位置に検索ボックスの`<div>`が埋め込まれる。

検索ボックスが正常に動いているページを調べると、`#__gcse_0`みたいなidの要素が追加されているっぽい

検索ボックス本体は`gsc-control-searchbox-only`みたいなやつらしい。このへんは検索ボックスの種類によって違うので適宜読み替えて。

そのあたりのキーワードを手がかりに調べてみる。

456行目

```javascript
...this.mc==google.search.B.pg&&(this.Na=b.Na)):this.cb=google.search.B.Bc:this.cb=google.search.B.Bc;this.root=J(this.Na?"gsc-control-searchbox-only":"gsc-control");this.root.dir=google.search.V.di;...
```

あやしい

266行目

```javascript
function J(a){var b=document.createElement("div");a&&(b.className=a);return b}
```

ということなので、456行目で検索ボックスの本体が作られているはず。


562行目

```javascript
b=this.root;this.Na||(b=J(),this.root.parentNode&&this.root.parentNode.insertBefore(b,this.root),y(b,this.root)
```

ここで`.gsc-control-searchbox-only`が追加されている?

`appendChild`を上書きして、どんなノードが追加されているか見てみた。

```html
    <script>
     (function () {
       var wrap = (cls, name, wrapper) => {
         var original = cls.prototype[name];
         cls.prototype[name] = wrapper(original);
       };

       wrap(Node, 'appendChild', (appendChild) =>
         function (child, ref) {
           console.log('appendChild', child.tagName);
           appendChild.call(this, child, ref);
         });

       wrap(EventTarget, 'addEventListener', (addEventListener) =>
         function (type, listener, cap) {
           console.log('addEventListener', type);
           addEventListener(type, listener, cap);
         });
     })();
    </script>
```

これを検索ボックスのスクリプトの前において、問題のページを再読込。

すると、最後に追加されたのは`<g:test>`という要素だった。

これは657行目で追加されている。

```javascript
l.Ot=function(){if("boolean"==typeof this.gm)return this.gm;var a=document.createElement("div");a.appendChild(document.createElement("G:TEST"));return this.gm=!!(a.querySelectorAll&&0<a.querySelectorAll("G\\:TEST").length)};
```

さらに一行前

```javascript
l=google.search.F.element.pn.prototype;
```

`google.search.F.element.pn.prototype.Ot`が呼ばれているところを探す

658行目

```javascript
l.go=function(a){a=(a?Xe(a):null)||document.body;a=this.Ot()?a.querySelectorAll(google.search.F.element.U.os):google.search.F.element.U.tu(a);window||(window={});window.___gcse_nc_=a.length||0;for(var b=0;b<a.length;++b)this.qq(new google.search.F.element.Yf(a[b]));this.Vo()};
```

こんどはl.go

664行目
ここでpnの要素が作られている

```javascript
google.search.F.element.vh=function(a){return google.search.F.element.dh?!1:(google.search.F.element.dh=new google.search.F.element.pn(a),google.search.F.element.Ow=google.search.F.element.oh(),!0)};qa("google.search.cse.element.init",google.search.F.element.vh,void 0);
```

dhがpnの要素であるらしい。

665

```javascript
google.search.F.element.go=function(a){google.search.F.element.vh();var b=google.search.F.element.oh();google.search.F.element.dh.go(a);google.search.F.element.sq("go",b)};qa("google.search.cse.element.go",google.search.F.element.go,void 0);
```

ここでF.element.go -> dh.go()が呼ばれている?

qaはwindowに指定したオブジェクトを追加するやつ。120行目辺りにある。

すくなくとも、ここまでは実行できていそう。

ここから下のソースコード全体の行数はたった3行なので、気合で読めば原因を特定できるはず。


sqは

663-664

```javascript
google.search.F.element.sq=function(a,b){if(!google.search.F.element.To&&(google.search.F.element.To=google.search.F.element.oh(),window.googleLT_&&1==window.googleLT_%100)){var c="e",d=window.googleLT_;window.__gcse&&window.__gcse.ct&&(d=window.__gcse.ct,c="c");var f=window.googleLT_-d,e=google.search.F.element.Sj-d,g=google.search.F.element.Ow-d;b-=d;d=google.search.F.element.To-d;if(0<e&&0<g&&0<b){var h="ex";"c"==c&&"explicit"!=window.__gcse.parsetags&&(h="ol");a="render"==a?"r":"g";var k=[];"c"==
c&&k.push("gl."+f);k=k.concat(["el."+e,"mc."+g,a+"s."+b,a+"e."+d]);google.loader.recordCsiStat(["element_"+(c+h)],k)}}};
```

ちょっと方法を変えてみる。
ページのスクリプトを少しずつ消していって、どこまで消した段階で動くようになるか。

まずは記事一覧を動かすために読み込んでいるスクリプトをすべてコメントアウトしてみる。
→動かない。なぜ？？？？？

次に`<script>`タグを全部コメントアウトしてみる。
これでも動かない。本当に謎。

検索バーのスクリプトは他のスクリプト`cse.js`を引っ張ってきてDOMに追加する。
通常の記事ページは`<head>`の中に`cse.js`を読み込むタグが追加されるっぽい。

記事一覧ページでは`<section>`内の`<script>`の隣にスクリプトが追加される位置が変わる。
これが何かまずいことを引き起こしている?

しかし、例えば[/ku/q-learning-demo/](/ku/q-learning-demo/)も`<section>`内に`cse.js`を読み込む`<script>`がおいてあるが、こちらは正常に動作する。これだけが原因とは考えられない。

その後、サイトのデザインを弄っていたらあるタイミング以降どのページでも検索バーがでなくなっていることに気づいた。
コミットの間を手動二分探索して、問題のコミットを特定
しかし、そのコミットは記事を一つ追加しているだけだった。

[/2017/07/24/01-emacs-emoji.html](/2017/07/24/01-emacs-emoji.html)

てっきり問題はどこかの`<script>`タグにあると思っていたので、以外

もしかすると、jemojiが絵文字に見えるようなJSのコードを勝手に変換している…？？
コロン使う所とかもあるし怪しい

というわけで、サイト内検索用の埋め込みスクリプトの部分がどのようにレンダリングされてるか確認し直してみると……

```html
    <script>
     (function() {
       var cx = 'XXXXXXX';
       var gcse = document.createElement('script');
       gcse.type = 'text/javascript';
       gcse.async = true;
       gcse.src = 'https://cse.google.com/cse.js?cx=' + cx;
       var s = document.getElementsByTagName('script')[0];
       s.parentNode.insertBefore(gcse, s);
     })();
    </script>
    <searchbox-only></searchbox-only>
```

ビンゴ

本来なら一番最後のタグは`<gcse:searchbox-only>`

jemojiに絵文字っぽい部分(`gcse:`)が消されてしまっている？

実際、HTMLの適当な場所に`<my:element>`みたいなタグを突っ込んでみたところ、`my:`の部分が消されてしまうことがわかった。

この部分全体を

```html
{% raw %}
{% raw %}
...
{% endraw %}
{% endraw %}
```

で囲ってみたが、効果なし。

jemojiのソースコードを読んでみると、

```ruby
Jekyll::Hooks.register [:pages, :documents], :post_render do |doc|
  Jekyll::Emoji.emojify(doc) if Jekyll::Emoji.emojiable?(doc)
end
```

ここが原因。絵文字の書き換えのタイミングが`:post_render`なので、rawとか関係なしにemojifyされてしまう。

というか、そもそもjekyllの仕様としてプラグインによるフィルタリングはレンダリング前か後にしかできないので、rawとか関係ない。

だからコロンつきのhtmlタグも勝手に絵文字だと思いこんで変換されている？？

と思ったけど、jemojiが内部で呼んでいる`Html::Pipeline::EmojiFilter`はちゃんと`:.*:`とかじゃなくてちゃんと特定の絵文字にマッチする正規表現(`:(smlie|joy|cat|..):`みたいなの)を生成してるので、コロンで始まってるだけとかでは誤処理されないはず。

何にせよ、テンプレートのレンダリングのタイミングでJekyll周りの何者かに書き換えられてしまっていることは確か

ここでJekyllなりjemojiなりを修正したところで、変更がマージされてGithub Pagesが公式に修正済みのバージョンを使うようになるまで待たないと行けない。

とりあえず場当たり的な対応。`#search-box`みたいな適当な`div`タグを検索ボックスを埋め込みたいところにおいておいて、

```javascript
     (function() {
       var insertSearchBox = () => {
         var sb = document.getElementById('search-box');
         sb.appendChild(document.createElement('gcse:searchbox-only'));
       };
       if (document.readyState != 'loading') {
         insertSearchBox();
       } else {
         document.addEventListener('DOMContentLoaded', () => insertSearchBox());
         }
     })();
```

これでとりあえずは解決。
