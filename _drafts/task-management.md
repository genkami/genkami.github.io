---
layout: post
title: タスク管理方法を徹底的に見直して留年回避した話
tags:
- Others
---

留年本当に回避できたら書く

## 現状の問題点

+ タスクを忘れる．とにかく忘れる．風呂はいっててシャンプー切れてるのに気づいても風呂上がる頃には忘れてるのを一週間繰り返すくらい忘れる．
+ この前は新居の契約書類をポストに入れるのを一ヶ月忘れてて危うく来年からの家がなくなる所だった．
+ タスクの優先順位を考えられない．タスクが大量に積もってると何故か全然重要じゃないところからやりだす．あと一番重要なやつは忘れてる．
+ そもそもタスク管理自体が続かない．1週間あれば1日は全くタスク一覧を見ない日ができる．1日経ってタスク一覧を見ると機能のデイリールーチンで埋め尽くされてて優先度高いものを忘れる．


## というわけで必要なもの

+ 忘れる前にタスクを入力できる簡単さ
+ タスクの一覧性(一画面に収まりきらないものはもれなく忘れる)
+ タスクを自由に並べ替える機能
+ タスクの繰り返し機能(デイリールーチン的なやつ)
+ デイリールーチンをこなせなかった時に次の日に繰り越したりできる機能があるとなおよい
+ 「期限はないけどいつかやっておかないといけないタスク」「いつかやりたいこと」をうまいこと管理する方法もほしい


## 試したもの

### 手帳

タスク入力→そんなに簡単じゃない．四六時中手帳持ち歩いているわけじゃないし，四六時中筆記用具を持ち歩いてるわけでもない．家の中とか講義受けてるときだけに限ればそれでもいいのかもしれないけど，日常生活全般で使うには不便．

一覧性→自分の好きなように書けるので，一覧性が高くなるようにメモしておけば高い

並べ替え→できない．最初に書いたことが消せないのは致命的．フリクション使えばいいじゃんっていう話だけど試される大地北海道では消したはずのインクの色が寒さで元に戻ってしまう可能性があるので，道民としては微妙．

繰り返し→手動でならできる．

やりたいこと系の管理→手帳の最後のページとかにやりたいことリスト書いておくと捗るかもね．

就活の時期に一瞬手帳を使い始めたんだけどその直後から予定管理で致命的な問題が発生しまくったので1ヶ月くらいでやめた．たぶん自分には向いてない．

### Remember The Milk
タスク入力→スマホから入力できるのでタスク入力は速い．現代の生活で一日のうちスマホが自分の半径2メートル以内にないことなんてまずない．Web版，Android版，iPhone版があるので，環境を選ばない．

一覧性→タスクの表示がコンパクトなので一覧性は高い．ただし，並び替えとかはなかった．基本的に期限が近いものが上に表示されていくだけ．

繰り返し→繰り返し設定をしたタスクは完了するしないにかかわらず次の日になったら増えているので，数日見ないとデイリールーチンで埋め尽くされて大変なことになってる．

いつかやる系の管理→リストをわけられるので「いつかやる」リストとか「やりたい」リストを作っておけばこれらの管理もできる．

### Habitica
タスク管理をゲーミフィケーションしてる面白いWebサービス．こちらもWeb版とiPhone, Android版があるので，どの環境でも動く．
タスクが「Habit」「Daily」「Todo」の3種類に分けられているのが◎

+ Habit: 習慣づけたいこと．「30分ジョギングする」とか「スマホを見ない」とか
+ Daily: 一日一回とか一週間に一回とかのタスク
+ Todo: 一回限りのもの

これらをこなしていくと経験値がたまってレベルアップしたり，ゲーム内通貨を手に入れて装備やペットを揃えることができる．逆にタスクを終わらせないとHPが減っていって最終的には死ぬ．

タスク入力→RTMと同様楽．

一覧性→Habit, Daily, Todoのそれぞれが仕組みとして完全に分かれてしまっており，これらを1画面内に同時に表示することが(Web版以外は)できない．

並べ替え→タスクの並び順は不明．並び替えはできない模様．難易度のパラメータを設定してフィルタリングはできるけど，細かい部分の融通は効かない．

繰り返し→デイリールーチンの繰り返し機能については神．一日経つとその日に終わらなかったDailyはすべて消えて，状態がリセットされる．
やりたいこと系の管理→タスクのフォルダ分けみたいなことはできない．なのでいつかやりたいけどそんなに優先度高くないことを書いておくといつまでも画面上に残り続けて邪魔．

### Trello
具体的な運用法も

+ ボード→大カテゴリ
+ リスト→中カテゴリ
+ カード→Todoとかを書いたやつ

基本的に1画面には1つのボードしか入らない(スマホなら1リストのみ)

Todo, Projects, BucketList, Dailyという4つのボードをメインに使っていく

+ Todo: 今日もしくは手が空き次第やっていかないといけないことを放り込む．基本的に朝起きたらTodo内のタスクを上から順に処理していけばいい状態に持っていく．
+ Projects: 長期的なタスクと，それを完了させるために必要な細々としたタスクを羅列しておく．毎日寝る前に確認して，明日手を付けられそう/手を付けなければならないものをTodoに移動
+ BucketList: 趣味開発とか積読とかの，いつかやりたいけど具体的な期限が定まっているわけでもないものを突っ込んでおく．これも毎日寝る前に確認して，明日やれそうなものはTodoに移動
+ Daily: 毎日とか毎週やるもの．ゴミ出しとか．これも毎日寝る前に確認して，明日やるものをTodoにコピー

タスク入力→RTM, Habiticaと比べると若干面倒．アプリ起動してボードとリスト選んでってやる一手間が入る．常時起動しておけばあんまり問題ないかも．

一覧性→「その日やることを全部一つのボードにまとめておく」という制約のもとでならそれなりにいい．ただし，1つのボードにリストが4〜5個くらいあるとPCじゃないと見るのが辛くなってくる．「困ったときはとりあえずこのリストを見る!」用を一つ作っておくと便利

並べ替え→自由にできる．予めタスクを優先度順に並べておいて，その後は思考停止して上からこなしていくだけみたいな運用が楽．

繰り返し→手動でやるしかない．ただボードをまるごとコピーする機能とかはあるので，これを使えば手間はそこまで大きくない．一応繰り返し機能もあるがこれをやるとRTMと同じ問題が発生する．

やりたいこと系の管理→BucketListボードにとりあえず放り込んでおいて，一日の終わりに眺めてやれそうなのをTodoに戻すっていう運用が楽．

全体的に元々の機能は微妙だけど運用でカバーできる範囲が大きいので，どうしようもないほどの欠点は生まれにくい．

ただし，基本的にどの操作も他のTodo管理系アプリよりは一手間かかる印象(とくにスマホ版だと)．



最後にミクアカで作ってるアプリの宣伝をいれる
