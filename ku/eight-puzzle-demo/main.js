(function () {

  // 諸事情により https://github.com/adamhooper/js-priority-queue とAPI互換
  // 実装は雑だし効率も良くない
  function PriorityQueue(args) {
    this.comperator = args.comperator;
    this.elems = args.initialValues;
    this.length = this.elems.length;
  }

  window.PriorityQueue = PriorityQueue;

  PriorityQueue.prototype.queue = function (x) {
    this.elems.push(x);
    this.length = this.elems.length;
  };

  PriorityQueue.prototype.dequeue = function () {
    if (this.elems.length == 0) throw 'empty queue';
    this.elems.sort(this.comperator);
    var ret = this.elems.shift();
    this.length = this.elems.length;
    return ret;
  };

  // js-priority-queue にはないメソッド
  // pred を満たす要素を一つポップして返す
  // 一つもない場合は null
  PriorityQueue.prototype.popElem = function (pred) {
    for (var i = 0; i < this.elems.length; i++) {
      if (pred(this.elems[i])) {
        var ret = this.elems.splice(i, 1)[0];
        this.length = this.elems.length;
        return ret;
      }
    }
    return null;
  };


  PUZZLE_WIDTH = 3;
  PUZZLE_HEIGHT = 3;

  // パズルの各セルに
  // 0 1 2
  // 3 4 5
  // 6 7 8
  // というように番号を割り振る。
  // パズルのピーズ a, b, c, d, e, f, g, h, i がそれぞれ
  // a b c
  // d e f
  // g h i
  // というように並んでいたとき、
  // 文字列 "abcdefghi" を用いてこの盤面を表す。
  // 上のような文字列を puzzlestr と呼ぶ。
  // パズルの盤面の位置 (x, y) (左上が原点) は puzzlestr の位置 (PUZZLE_WIDTH * x + y)
  // で表される。
  function Puzzle(puzzlestr, depth, empty_pos_x, empty_pos_y) {
    this.puzzlestr = puzzlestr;
    this.empty_x = empty_pos_x;
    this.empty_y = empty_pos_y;
    this.depth = depth;
  };
  window.Puzzle = Puzzle;

  Puzzle.width = PUZZLE_WIDTH;
  Puzzle.height = PUZZLE_HEIGHT;

  Puzzle.toStrIndex = (x, y) => {
    return (PUZZLE_WIDTH * y) + x;
  };

  Puzzle.fromStrIndex = (i) => {
    return [i % PUZZLE_WIDTH, Math.floor(i / PUZZLE_WIDTH)];
  };

  // (x, y) を空きの位置として、 (a, b) のピースと (x, y) のピースを
  // 入れ替えた盤面を返す
  // ただし、 (a, b) が盤面の外を参照している場合は null を返す
  Puzzle.prototype.swap = function (a, b) {
    if (a < 0 || PUZZLE_WIDTH <= a || b < 0 || PUZZLE_HEIGHT <= b) {
      return null;
    }
    var s = Puzzle.toStrIndex(this.empty_x, this.empty_y);
    var t = Puzzle.toStrIndex(a, b);
    var newpzlstr = '';
    for (var i = 0; i < this.puzzlestr.length; i++) {
      if (i == s) {
        newpzlstr += this.puzzlestr[t];
      } else if (i == t) {
        newpzlstr += this.puzzlestr[s];
      } else {
        newpzlstr += this.puzzlestr[i];
      }
    }
    return new Puzzle(newpzlstr, this.depth + 1, a, b);
  };

  // 現在の状態から一手動かしたものとして可能な盤面をすべて返す
  Puzzle.prototype.possibleNextSteps = function () {
    var x = this.empty_x;
    var y = this.empty_y;
    var dps = [[1, 0], [-1, 0], [0, 1], [0, -1]];
    var result = [];
    dps.forEach((dp) => {
      var next = this.swap(x + dp[0], y + dp[1]);
      if (next) {
        result.push(next);
      }
    });
    return result;
  };

  // 正解の盤面
  // E が空きのセル
  Puzzle.goal = new Puzzle("01234567E", 0, 2, 2);

  // ヒューリスティック関数 h を用いて IDA* アルゴリズムにより盤面の検索を1ステップ行う。
  // この際、探索に優先度付きキュー pq を用いる。
  // 副作用として、 pq, visited の値を更新する。
  Puzzle.idaStarStep = function (puzzle, h, pq, visited, goal, max_depth) {
    puzzle.possibleNextSteps().forEach((p) => {
      if (p.puzzlestr in visited) {
        elm = pq.popElem((elm) => elm.p.puzzlestr == p.puzzlestr);
        if (elm && p.depth > elm.p.depth) {
          pq.queue(elm);
        } else if (elm) {
          pq.queue({ p: p, h: p.depth + h(p) });
        }
      } else if (p.depth <= max_depth) {
        pq.queue({ p: p, h: p.depth + h(p) });
        visited[p.puzzlestr] = true;
      }
    });
  };

  // ヒューリスティック関数 h を用いて A* アルゴリズムにより盤面の検索を1ステップ行う。
  // この際、探索に優先度付きキュー pq を用いる。
  // 副作用として、 pq, visited の値を更新する。
  Puzzle.aStarStep = function (puzzle, h, pq, visited, goal) {
    puzzle.possibleNextSteps().forEach((p) => {
      if (p.puzzlestr in visited) {
        elm = pq.popElem((elm) => elm.p.puzzlestr == p.puzzlestr);
        if (elm && p.depth > elm.p.depth) {
          pq.queue(elm);
        } else if (elm) {
          pq.queue({ p: p, h: p.depth + h(p) });
        }
      } else {
        pq.queue({ p: p, h: p.depth + h(p) });
        visited[p.puzzlestr] = true;
      }
    });
  };


  Puzzle.heuristic = {};
  // 盤面 goal を渡すと、 goal と一致しないマスの数を数えるヒューリスティック関数を返す
  Puzzle.heuristic.countIncorrectPiece = (goal) => {
    return (p) => {
      var countIncorrect = 0;
      for (var i = 0; i < goal.puzzlestr.length; i++) {
        if (goal.puzzlestr[i] != p.puzzlestr[i]) {
          countIncorrect++;
        }
      }
      return countIncorrect;
    };
  };

  // 盤面 goal を渡すと、 goal とのマンハッタン距離を返すヒューリスティック関数を返す
  Puzzle.heuristic.manhattan = (goal) => {
    return (p) => {
      var dist = 0;
      for (var i = 0; i < goal.puzzlestr.length; i++) {
        var c = goal.puzzlestr[i];
        var j = 0;
        for (; j < p.puzzlestr.length; j++) {
          if (p.puzzlestr[j] == c) {
            break;
          }
        }
        var a = Puzzle.fromStrIndex(i);
        var b = Puzzle.fromStrIndex(j);
        dist += Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]);
      }
      return dist;
    };
  };
})();
