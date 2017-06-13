<main-view>
  <form onSubmit={ runSearch }>
    <select value={ algIndex }>
      <option each={ alg, i in algorithms } value={ i }>{ alg.desc }</option>
    </select>
  </form>
    <div>
      { algorithms[algIndex].desc }
    </div>
  <script>
   this.algorithms = [
     { desc: "A* (推定関数: 位置が正しくないタイルの数)" },
     { desc: "A* (推定関数: マンハッタン距離)" },
     { desc: "IDA* (推定関数: 位置が正しくないタイルの数, cutoff は１ずつ増加)" },
     { desc: "IDA* (推定関数: マンハッタン距離, cutoff は１ずつ増加)" }
   ];
   this.algIndex = 0;
  </script>
</main-view>
