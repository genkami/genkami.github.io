---
layout: post
title: 'position: fixedがiPhoneで効かない?'
tags:
- CSS
---

[ライフゲーム](/game-of-life/)の改修中に起きた事件

CSS初心者なのでアホみたいなことで詰まってるかも

製作中の画面(PC上)

![/img/post/2017-07-28-poisition-fixed-01.png](/img/post/2017-07-28-poisition-fixed-01.png)

このように一番下にボタンが固定されるはず

しかしiPhoneから見てみると、

![/img/post/2017-07-28-poisition-fixed-01.png](/img/post/2017-07-28-poisition-fixed-01.png)

このように、固定されているはずのボタンが画面外に置かれてしまう。

ちなみに、メニュー部分のCSSがこれ

```css
   .open-menu-button {
     background-color: #ff2c00;
     color: white;
     font-weight: bold;
     width: 50px;
     height: 50px;
     border-radius: 50%;
     box-shadow: 0px 0px 4px 0px #252525;
   }
```



