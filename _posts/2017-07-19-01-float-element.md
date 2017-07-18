---
layout: post
title: 親要素の高さを子のfloatしている要素に合わせる
tags:
- CSS
---

つい先ほどまで、このサイトのタグ一覧は画面幅が狭い時に悲惨なことになっていました。

![/img/post/2017-07-19-float-01.png](/img/post/2017-07-19-float-01.png)

この原因は、タグ一覧にfloatされている`li`タグを使っていることです。

現在のタグ一覧の部分は以下のような構成になっています。

```html
<div.tag-list>
  <ul>
    <li>タグ名</li>
  </ul>
</div>
```

`li`タグがfloatされているため、`div.tag-list`の高さはほとんどなくなり、浮いた`li`タグが下の記事タイトルの部分にまで侵略してしまっています。

これを解消するために、`div.tag-list`の末尾に次のように要素を追加しました。

```css
.tag-list:after {
    content: "";
    display: block;
    clear: both;
}
```

こうすることによって、`:after`の終わりまで`div.tag-list`で覆うことができ、さらに`clear: both`しているため`li`タグのfloatを打ち消すことができます。

![/img/post/2017-07-19-float-02.png](/img/post/2017-07-19-float-02.png)

このように、綺麗に高さを揃えることができるようになりました。
