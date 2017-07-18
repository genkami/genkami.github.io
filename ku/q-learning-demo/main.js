(function () {
  var mapWidth = 30;
  var mapHeight = 15;

  var cellSize = (canvas) => {
    return {
      width: Math.floor(canvas.width / mapWidth),
      height: Math.floor(canvas.height / mapHeight)
    };
  };

  // 決め打ち
  var isBlock = (x, y) => {
    return (0 <= x && x <= 26 && 0 <= y && y <= 4) ||
           (0 <= x && x <= 3 && 9 <= y && y < mapHeight) ||
           (4 <= x && x <= 7 && 10 <= y && y < mapHeight) ||
           (8 <= x && x <= 11 && 12 <= y && y < mapHeight) ||
           (x < 0 || mapWidth <= x || y < 0 || mapHeight <= y);
  };

  var isGoal = (x, y) => {
    return (26 <= x && x <= mapWidth && y == 0);
  };

  var initialState = () => {
    return { x: 0, y: 5 + Math.floor(Math.random() * 4), dx: 0, dy: 0 };
  };

  var makeActions = () => {
    var actionId = 0;
    var actions = [];
    for (var i = -1; i <= 1; i++) {
      for (var j = -1; j <= 1; j++) {
        // 加減速しないことは許されない
        if (i != j) {
          actions.push({ id: actionId, ddx: i, ddy: j, pFail: 0.3 });
          actionId++;
        }
      }
    }
    return actions;
  };

  var actions = makeActions();

  // 行動を実行したときのコスト
  var costs = {
    takeAction: 10, // 通常時
    offTrack: 100, // コースアウトした
    reachGoal: -20 // ゴールに着いた
  };

  // 状態 state から行動 action を取って状態 newState に遷移するのにかかるコストを返す
  var getCost = (state, newState, action) => {
    if (isBlock(newState.x, newState.y)) {
      return costs.offTrack;
    } else if (isGoal(newState.x, newState.y)) {
      return costs.reachGoal;
    } else {
      return costs.takeAction;
    }
  }

  // 状態 state で行動 action を取った場合の遷移先を返す。
  // ただし、遷移には action.pFail の確率で失敗する。
  // また、ブロックにぶつかったら最初からやり直し。
  var nextState = (state, action) => {
    if (Math.random() < action.pFail) {
      return state // 失敗
    }
    var newState = {
      x: state.x + state.dx + action.ddx,
      y: state.y + state.dy + action.ddy,
      dx: state.dx + action.ddx,
      dy: state.dy + action.ddy
    };
    if (isBlock(newState.x, newState.y)) {
      return initialState();
    } else {
      return newState;
    }
  };

  // 学習率
  // state, action に依存する値でもよいが、とりあえずこの値でおいている
  var learningRate = (t, state, action) => 1/t;

  // 割引率
  var discount = 0.6;

  // Q-learning の Q
  var makeQ = () => {
    // 5次元配列(?)
    // QTable[x][y][dx][dy][actionId]
    // dx, dy に対応する次元は値の範囲が微妙な感じなので、文字列化してオブジェクトに放り投げる
    var QTable = [];

    // Q の値を取得。値が存在しない場合は初期値が返されれる
    var getQ = (state, action) => {
      if (QTable[state.x] === undefined) {
        QTable[state.x] = [];
      }
      if (QTable[state.x][state.y] === undefined) {
        QTable[state.x][state.y] = {};
      }
      if (QTable[state.x][state.y][state.dx] === undefined) {
        QTable[state.x][state.y][state.dx] = {};
      }
      if (QTable[state.x][state.y][state.dx][state.dy] === undefined) {
        QTable[state.x][state.y][state.dx][state.dy] = [];
      }
      if (QTable[state.x][state.y][state.dx][state.dy][action.id] === undefined) {
        QTable[state.x][state.y][state.dx][state.dy][action.id] = costs.takeAction;
      }
      return QTable[state.x][state.y][state.dx][state.dy][action.id];
    }

    // ラウンド t で state から action を実行して nextState に遷移したとして、 Q の値を更新する
    var updateQ = (t, state, nextState, action) => {
      var a = learningRate(t, state, action);
      var minNextQ = Math.min.apply(Math, actions.map((action) => getQ(nextState, action)));
      var oldQ = getQ(state, action);
      var cost = getCost(state, nextState, action);
      QTable[state.x][state.y][state.dx][state.dy][action.id] =
        (1 - a) * oldQ + a * (cost + discount * minNextQ);
    };

    return { get: getQ, update: updateQ };
  };

  var Q = makeQ();

  // 状態 state で取れる行動のうち、最も Q の値が低いものを返す
  var getMostEffectiveAction = (state) => {
    var minQ = 1/0;
    var effecviveAction = null;
    actions.forEach((action) => {
      var q = Q.get(state, action);
      if (q < minQ) {
        minQ = q;
        effecviveAction = action;
      }
    });
    return effecviveAction;
  };

  var round = 1; // 学習のラウンド
  var state = initialState(); // 現在の状態
  var bgCanvas, carCanvas; // キャンバス。ロード後に設定。

  // 行動を一つ選択して実行し、次のラウンドに移る
  var doRound = () => {
    if (isGoal(state.x, state.y)) state = initialState();

    var action = getMostEffectiveAction(state);
    var newState = nextState(state, action);
    Q.update(round, state, newState, action);
    state = newState;
  };

  var drawBg = (canvas) => {
    var cell = cellSize(canvas);
    var ctx = canvas.getContext('2d');
    for (var i = 0; i < mapWidth; i++) {
      for (var j = 0; j < mapWidth; j++) {
        if (isBlock(i, j)) {
          ctx.strokeStyle = 'gray';
          ctx.fillStyle = 'gray';
          ctx.beginPath();
          ctx.fillRect(i * cell.width, j * cell.height, cell.width, cell.height);
          ctx.closePath();
        } else if (isGoal(i, j)) {
          ctx.strokeStyle = 'orange';
          ctx.fillStyle = 'orange';
          ctx.beginPath();
          ctx.fillRect(i * cell.width, j * cell.height, cell.width, cell.height);
          ctx.closePath();
        }
      }
    }

    // 枠
    ctx.strokeStyle = 'black';
    ctx.beginPath();
    ctx.strokeRect(0, 0, canvas.width, canvas.height);
    ctx.closePath();
  };

  var drawCar = (canvas, state) => {
    var cell = cellSize(canvas);
    var ctx = canvas.getContext('2d');
    ctx.strokeStyle = 'red';
    ctx.fillStyle = 'red';
    ctx.beginPath();
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.fillRect(state.x * cell.width, state.y * cell.height, cell.width, cell.height);
  };

  var frame = () => {
    doRound();
    drawCar(carCanvas, state);
  };
  var intervalId = null;
  var interval = 200;

  var init = () => {
    bgCanvas = document.querySelector('#bg-1');
    carCanvas = document.querySelector('#car-1');
    var button = document.getElementById('round-100000');
    button.addEventListener('click', () => {
      clearInterval(intervalId);
      for (var i = 0; i < 100000; i++) {
        doRound();
      }
      setInterval(frame, interval);
    });
    drawBg(bgCanvas);
    drawCar(carCanvas, state);
    intervalId = setInterval(frame, interval);
  };

  if (document.readyState !== 'loading') {
    init();
  } else {
    document.addEventListener('DOMContentLoaded', init);
  }

})();
