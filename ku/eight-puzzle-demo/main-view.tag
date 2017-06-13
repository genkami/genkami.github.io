<main-view>
  <puzzle board={ puzzle }></puzzle>
  <form onSubmit={ runSearch }>
    <select value={ algIndex }>
      <option each={ alg, i in algorithms } value={ i }>{ alg.desc }</option>
    </select>
    <input type="submit" value="サーチを実行する">
  </form>
    <div>
      { algorithms[algIndex].desc }
    </div>
    <div>
      節点展開回数: { how_many_loop },
      保存している節点の最大数: { max_queue_size },
      盤面の深さの最大値: { max_depth }
      <button onClick={ cancel }>中止</button>
    </div>
    <div>
      { message }
    </div>
    <script>
     this.algorithms = [
       { desc: "A* (推定関数: 位置が正しくないタイルの数)",
         type: 'A*',
         h: 'count' },
       { desc: "A* (推定関数: マンハッタン距離)",
         type: 'A*',
         h: 'manhattan' },
       { desc: "IDA* (推定関数: 位置が正しくないタイルの数, cutoff は１ずつ増加)",
         type: 'IDA*',
         h: 'count' },
       { desc: "IDA* (推定関数: マンハッタン距離, cutoff は１ずつ増加)",
         type: 'IDA*',
         h: 'manhattan' }
     ];
     this.algIndex = 0;
     this.initial = new Puzzle('304E21675', 0, 0, 1);
     this.goal = new Puzzle('012345678E', 0, 2, 2);
     this.message = '';
     this.cmp = (a, b) => a.h - b.h;
     this.interval = 1;

     makeInitialPQ() {
       return new PriorityQueue({
         comperator: this.cmp,
         initialValues: [{ p: this.initial, h: 0 }]
       });
     }

     initialize() {
       this.visited = {}
       this.visited[this.initial.puzzlestr] = true;
       this.puzzle = this.initial;
       this.pq = this.makeInitialPQ();
       this.cutoff = 0;
       this.how_many_loop = 0;
       this.max_queue_size = 0;
       this.max_depth = 0;
     }

     this.initialize();

     runSearch(e) {
       e.preventDefault();
       this.alg = this.algorithms[this.algIndex];
       if (this.alg.h === 'manhattan') {
         this.h = Puzzle.heuristic.manhattan(this.goal);
       } else {
         this.h = Puzzle.heuristic.countIncorrectPiece(this.goal);
       }
       this.initialize();
       this.timer = setInterval(this.step, this.interval);
     }

     step() {
       if (this.pq.length == 0) {
         if (this.alg.type == 'A*') {
           console.log('nannyate');
           this.message = '探索に失敗しました: 存在し得ない盤面です。';
           clearInterval(this.timer);
           this.update();
           return;
         } else {
           // IDA*
           console.log('fukakusuru');
           this.pq = this.makeInitialPQ();
           this.cutoff++;
           this.visited = {};
           this.visited[this.initial.puzzlestr] = true;
         }
       }
       this.max_queue_size = Math.max(this.max_queue_size, this.pq.length);
       var first = this.pq.dequeue();
       this.puzzle = first.p;
       this.how_many_loop++;
       this.max_depth = Math.max(this.max_depth, first.p.depth);

       if (first.p.puzzlestr === this.goal.puzzlestr) {
         this.message = '探索が終了しました。';
         clearInterval(this.timer);
         this.update();
         return;
       }

       if (this.alg.type == 'A*') {
         Puzzle.aStarStep(first.p, this.h, this.pq, this.visited, this.goal);
       } else {
         // IDA*
         Puzzle.idaStarStep(first.p, this.h, this.pq, this.visited, this.goal, this.cutoff);
       }
       this.update();
     }

     cancel() {
       clearInterval(this.timer);
       this.initialize();
       this.update();
     }
    </script>
</main-view>
