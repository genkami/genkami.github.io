<puzzle-cell>
  <img src={ imageUrl } style="margin: 0px;">

  <style>
    width: 169px;
    height: 169px;
  </style>

  <script>
   toImageUrl(char) {
     return './img/puzzle-' + char + '.jpg';
   }

   this.imageUrl = this.toImageUrl(opts.char);

   this.on('updated', () => {
     this.imageUrl = this.toImageUrl(opts.char);
   });
  </script>
</puzzle-cell>
