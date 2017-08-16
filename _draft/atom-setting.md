起動時に出てくるWelcome Guideを消す
起動時の画面で"Show Welcome Guide when opening Atom"のチェックを外す

Emacsとは違い、プロジェクト単位でAtomを一つ開く使い方が正解っぽい

## 基本的なキーバインド

### カーソル移動
+ `Command + ←→`: 行頭、行末に移動
+ `Option + ←→`: 次、前の単語に移動
+ `Command + ↑↓`: ファイル先頭、末尾に移動
+ `Ctrl + g`: 指定行に移動
+ `Ctrl + m`: 対応する開き括弧、閉じ括弧に移動
Macの場合はCtrl+pnfbae等がデフォルトで使えるのでEmacs感覚で使うこともできます

### 編集
+ `Ctrl + h`: カーソルの前の文字を消す
+ `Ctrl + d`: カーソルの後ろの文字を消す
+ `Option + BackSpace`: 単語の先頭までを削除
+ `Command + BackSpace`: 行頭まで削除
+ `Ctrl + k`: 行末まで削除
+ `Command + s`: 保存
+ `Command + c`: コピー
+ `Command + v`: ペースト
+ `Command + x`: 切り取り

### 検索/置換
+ `Command + f`: 検索バーを表示
+ `Command + Option + /`: 正規表現による検索のon/off
+ `Command + Option + c`: 大文字小文字を区別するかどうかの切り替え
+ `Command + Option + s`: 選択範囲のみを検索するかどうかの切り替え
+ `Enter`: 検索
+ `Option + Enter`: すべて検索(カーソルが一致部分全てに現れ、一括で編集できる)
+ `Command + Option + e`: 置換
+ `Commant + Enter`: すべて置換

### インデント
+ `TAB`: インデント
+ `Command + [`: インデントを減らす
+ `Command + ]`: インデントを増やす

### ファイル
+ `Command + O`: ファイルを開く
+ `Ctrl + \`: ツリービューの表示/非表示
+ `Ctrl + 0`: ツリービューに移動/ツリービューから戻る
+ `Ctrl + pn`: ツリービュー内で上下移動
+ `Enter`: ツリービュー内でディレクトリを展開/畳む
+ `a`: ツリービュー内でファイルを追加
+ `Shift + a`: ツリービュー内でディレクトリを追加
+ `BackSpace`: ツリービュー内でファイルを削除
+ `m`: ツリービュー上でファイル名の変更

### タブ/ペイン
+ `Ctrl + TAB`: タブ切り替え
+ `Ctrl + w`: タブを閉じる
+ `Command + k, ←↑↓→`: 指定した方向にペインを作る
+ `Command + k, Command + ←↑↓→`: ペイン間の移動
+ `Command + w`: ペインを閉じる

### Markdown
+ `Ctrl + Shift + m`: プレビュー

## 設定
設定画面は`Command + w`で開ける

### タブをスペースにする
