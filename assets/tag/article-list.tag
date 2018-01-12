<article-list>
  <h1>{ title }</h1>
  <article-entry each={ article in targetArticles } entry={ article }>
  </article-entry>

  <div class="article-nav">
    <div id="prev-page" style="float: left;" if={ prevPageExists }>
      <a href={ prevPageUrl } id="prev-page-link"><i class="fa fa-chevron-left"></i>新しい記事</a>
    </div>
    <div id="next-page" style="text-align: right;" if={ nextPageExists }>
      <a href={ nextPageUrl } id="next-page-link">古い記事<i class="fa fa-chevron-right"></i></a>
    </div>
  </div>

  <script>
   loadArticles() {
     fetch(opts.source).then((resp) => {
       return resp.json();
     }).then((articles) => {
       this.articles = articles.map((article) => {
         return {
           "title": this.decode(article.title),
           "date": article.date,
           "url": article.url,
           "tags" : article.tags.map((t) => this.decode(t)),
           "summary": this.decode(article.summary)
         };
       });
       this.trigger('articlesLoaded');
     });
   }

   decode (data) {
     return decodeURIComponent(data.replace('+', ' '));
   }

   showContents() {
     const articlesOfTag = this.filterTags(this.articles, opts.filters.tag);
     this.targetArticles = this.paginate(
       articlesOfTag, opts.filters.page, opts.filters.offset);
     this.prevPageExists = opts.filters.page > 0;
     this.nextPageExists =
       (opts.filters.page + 1) * opts.filters.offset < articlesOfTag.length;
     this.prevPageUrl = this.urlFor(opts.filters.tag, opts.filters.page - 1);
     this.nextPageUrl = this.urlFor(opts.filters.tag, opts.filters.page + 1);
   }

   filterTags(articles, tag) {
     return articles.filter((a) => tag === null ||
                                 a.tags.find((elt) => elt === tag));
   }

   paginate(articles, page, offset) {
     return articles.slice(page * offset, (page + 1) * offset);
   }

   urlFor(tag, page) {
     if (tag) return `#${tag}/${page}`;
     else return `#${page}`;
   }

   this.articles = [];
   this.targetArticles = [];
   this.prevPageExists = false;
   this.nextPageExists = false;

   if (this.opts.tag == null) {
     this.title = '記事一覧';
   } else {
     this.title = `タグ「{tag}」の記事一覧`;
   }

   this.on('articlesLoaded', () => {
     this.showContents();
     this.update();
   });

   this.on('update', () => {
     this.showContents();
   });

   this.loadArticles();
  </script>
</article-list>

