---
layout: post
title: Jekyllでタグごとの記事一覧ページを無理やり作る
tags:
- JavaScript
- Jekyll
---
Jekyllではタグ一覧や記事一覧は取得できてもタグごとの記事一覧を表示する機能がないので、無理やりそれっぽいのを作りました。

{% highlight html %}
{% raw %}
<h1>タグ '<span class="tag-name"></span>' の記事一覧</h1>
{% for tag in site.tags %}
<div class="tag tag-{{ tag[0] }}">
  <ul>
    {% for post in tag[1] %}
    <li><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></li>
    {% endfor %}
  </ul>
</div>
{% endfor %}

<div class="tag tag-unknown">
  <span class="tag-name"></span>にマッチする記事はありません。
</div>

<script src="https://code.jquery.com/jquery-3.2.1.min.js"></script>
<script>
 (function () {
   var filterTags = () => {
     $('.tag').hide();
     var currentTag = location.hash.substring(1);
     var articles = $('.tag-' + currentTag);
     $('.tag-name').text(currentTag);
     if (articles.length > 0) {
       articles.show();
     } else {
       $('.tag-unknown').show();
     }
   }

   $(document).ready(() => filterTags());
   $(window).on('hashchange', () => filterTags());
 })();
</script>
{% endraw %}
{% endhighlight %}

タグごとの記事一覧ページへのリンクは以下のように生成しています。

{% highlight html %}
{% raw %}
<ul>
  {% for tag in site.tags %}
  <li><a href="{{ site.baseurl }}/tags.html#{{ tag[0] }}">{{ tag[0] }}</a></li>
  {% endfor %}
</ul>
{% endraw %}
{% endhighlight %}

要はとりあえず全タグについての記事一覧を出力してしまって、あとからJSで必要ない部分を非表示にしている感じです。

雑ですが、忙しいのでとりあえずこれで。