<puzzle>
  <table border="1">
    <tr each={ y in [0, 1, 2] }>
      <td each={ x in [0, 1, 2] } style="margin: 0px; padding: 0px;">
        <puzzle-cell char={ puzzleChar(x, y); }></puzzle-cell>
      </td>
    </tr>
  </table>

  <script>
   this.puzzle = opts.board;

   this.on('updated', () => {
     this.puzzle = opts.board;
   });

   puzzleChar(x, y) {
     return this.puzzle.puzzlestr[Puzzle.toStrIndex(x, y)];
   }
  </script>
</puzzle>
