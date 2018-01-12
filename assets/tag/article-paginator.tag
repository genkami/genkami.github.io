<article-paginator>
  <article-list source={ src } filters={ filters }></article-list>
  <script>
   this.src = '/articles.json';
   this.filters = {
     tag: null,
     page: 0,
     offset: GlobalConfig.offset
   };
  </script>
</article-paginator>
