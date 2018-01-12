<article-paginator>
  <article-list source={ src } filters={ filters }></article-list>

  <div class="article-nav">
    <div id="prev-page" style="float: left;">
      <a href="" id="prev-page-link"><i class="fa fa-chevron-left"></i>新しい記事</a>
    </div>
    <div id="next-page" style="text-align: right;">
      <a href="" id="next-page-link">古い記事<i class="fa fa-chevron-right"></i></a>
    </div>
  </div>

  <script>
   urlFor(tag, page) {
     if (tag) return `#${tag}/${page}`;
     else return `#${page}`;
   }

   route(hash) {
     // #PAGE
     var match = hash.match(/^#(\d+)\/?$/);
     if (match) {
       return { tag: null, page: match[1] ? parseInt(match[1]) : 0 };
     }
     // #TAG/PAGE
     match = hash.match(/^#([^\/]+)(\/(\d*)\/?)?$/);
     if (match) return {
       tag: match[1] ? decodeURI(match[1]) : null,
       page: match[3] ? parseInt(match[3]) : 0
     };
     return { tag: null, page: 0 };
   };

   this.src = '/articles.json';
   this.filters = {
     tag: null,
     page: 0,
     offset: GlobalConfig.offset
   };

   opts.observable.on('HashChanged', () => {
     filters.offset = GlobalConfig.offset;
     this.filters = filters;
     console.log(this.filters);
   });
  </script>
</article-paginator>
