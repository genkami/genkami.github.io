<article-paginator>
  <article-list source={ src } filters={ filters }></article-list>

  <script>
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
     filters = this.route(location.hash);
     filters.offset = GlobalConfig.offset;
     this.filters = filters;
     console.log(this.filters);
     this.update();
   });
  </script>
</article-paginator>
