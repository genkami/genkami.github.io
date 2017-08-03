---
layout: post
title: Haskellで型を明示的に適用する
tags:
- Haskell
---

GHC8からの機能として、多相的な関数`f`に対して`f @T`とすることで明示的に型を指定することができるようになりました。

この機能は`TypeApplications`という名前で提供されています。

```haskell
Prelude> :set -XTypeApplications
Prelude> :t show
show :: Show a => a -> String
Prelude> show 3
"3"
Prelude> show @Double 3 -- 型変数 a を明示的に Double に指定している
"3.0"
```

これでCoqでいう`@f T`みたいなことができるようになりました。厳密には両者の機能は全く違いますが。
