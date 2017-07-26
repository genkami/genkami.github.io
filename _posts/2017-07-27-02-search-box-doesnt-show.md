---
layout: post
title: Googleカスタム検索ボックスが表示されない
tags:
- JavaScript
---

このサイトの記事一覧ページだけカスタム検索ボックスが表示されないことに気づきました(解決済)。

通常、Googleのカスタム検索ボックスはGoogleから発行される以下のようなコードを埋め込むことで表示できます。

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

しかし、同じスクリプトを使っているはずなのに記事一覧ページだけ表示されない…。記事一覧の表示件数などを制御しているスクリプトが何かと競合しているのかと思いそのスクリプトを一旦消してみるなどしてみましたが、効果はなし。完全に原因不明なので、しばらく保留していました。

その後、サイトのデザインを弄っていたところ、あるタイミング以降どのページでも検索バーがでなくなっていることに気がつきました。

今回はいつ頃からこの不具合が起こったかも把握できていたので、問題となりそうなコミットログの間を手動で二分探索して、問題のコミットを特定しました。

しかし、そのコミットは以下の記事を一つ追加しているだけのものでした。

[/2017/07/24/01-emacs-emoji.html](/2017/07/24/01-emacs-emoji.html)

てっきり問題はどこかの`<script>`タグにあると思っていたので、予想外の展開です。

この記事が他の記事と違う唯一の点は、他の記事ではほとんど使用されない絵文字を多用しているところです。

もしかすると、jemojiが絵文字に見えるようなJSのコード等を勝手に変換しているのでしょうか…？？

カスタム検索ボックスのタグは`<gcse:***>`という形になっており、なんとなく絵文字に判定されそうな気もするので怪しいといえば怪しいです。

というわけで、検索ボックスが表示されない場合にサイト内検索用の埋め込みスクリプトの部分がどのようにレンダリングされてるか確認し直してみると……

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

予想通り、一部分が勝手に書き換えられていることがわかりました。

本来なら一番最後のタグは`<gcse:searchbox-only>`になっているはずなのですが、何故か先頭の`gcse:`の部分が抜け落ちています。

jemojiにあたりに絵文字っぽい部分(`gcse:`)が消されてしまっているのでしょうか？

実際、HTMLの適当な場所に`<my:element>`みたいなタグを突っ込んでみたところ、`my:`の部分が消されてしまうことがわかりました。また、問題になる部分全体を`raw ... endraw`で囲ってみましたが、これも効果なし。というのも、jemojiのソースコードを読んでみると、

```ruby
Jekyll::Hooks.register [:pages, :documents], :post_render do |doc|
  Jekyll::Emoji.emojify(doc) if Jekyll::Emoji.emojiable?(doc)
end
```

というようになっており、絵文字の書き換えのタイミングが`:post_render`なので、`raw`とか関係なしにemojifyされてしまうようです。

これが原因でコロンつきのhtmlタグも勝手に絵文字だと思いこんで変換されているのでしょうか？？

しかし、jemojiが内部で呼んでいる`Html::Pipeline::EmojiFilter`は、ちゃんと特定の絵文字にマッチする正規表現(`/:(smlie|joy|cat|..):/`みたいなの)を生成し、それにマッチしている部分のみを置換しているので、コロンで始まってるだけとかでは誤処理されないはずです。

何にせよ、テンプレートのレンダリングのタイミングでJekyll周りの何者かに書き換えられてしまっていることは確かです。多分、`Html::Pipeline`のパーサあたりが原因なのではないでしょうか。

しかし、ここでJekyllなりjemojiなりを修正したところで、変更がマージされてGithub Pagesが公式に修正済みのバージョンを使うようになるまで待たなければなりません。

とうわけで、とりあえず場当たり的な対応。

`#search-box`みたいな適当な`div`タグを検索ボックスを埋め込みたいところにおいておいて、

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

というスクリプトで後から`gcse:`タグを挿入することにしました。これでとりあえずは解決。

暇な時に原因を調べてissueでも投げておきたいです。
