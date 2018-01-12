<article-list>
  <h1>{ title }</h1>
  <article-entry each={ article in targetArticles } entry={ article }>
  </article-entry>
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

   makeFilter(args) {
     return (articles) => articles
       .filter((a) => args.tag === null ||
                    a.tags.find((elt) => elt === args.tag))
       .slice(args.page * args.offset, (args.page + 1) * args.offset);
   decode (data) {
     return decodeURIComponent(data.replace('+', ' '));
   }

   }

   this.filter = this.makeFilter(opts.filters);
   this.articles = [];
   this.targetArticles = [];

   if (this.opts.tag == null) {
     this.title = '記事一覧';
   } else {
     this.title = `タグ「{tag}」の記事一覧`;
   }

   this.on('articlesLoaded', () => {
     this.targetArticles = this.filter(this.articles);
     this.update();
   });

   this.loadArticles();
  </script>
</article-list>

