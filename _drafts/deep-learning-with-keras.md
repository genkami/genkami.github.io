---
layout: post
title: 『Deep Learning with Keras』を読んだ
tags:
- Books
- Python
- 機械学習
---

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/B06Y2YMRDW/cloudear-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/510Z9dZUKyL._SL160_.jpg" alt="Deep Learning with Keras: Implementing deep learning models and neural networks with the power of Python" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/B06Y2YMRDW/cloudear-22/ref=nosim/" name="amazletlink" target="_blank">Deep Learning with Keras: Implementing deep learning models and neural networks with the power of Python</a><div class="amazlet-powered-date" style="font-size:80%;margin-top:5px;line-height:120%">posted with <a href="http://www.amazlet.com/" title="amazlet" target="_blank">amazlet</a> at 18.01.21</div></div><div class="amazlet-detail">Packt Publishing (2017-04-26)<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/B06Y2YMRDW/cloudear-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>


内容

+ Ch1 最低限必要な数学的知識についてさらっと．
+ Ch2 Kerasのインストール方法と，簡単な使い方の説明．GCPとかAWS, Azureで使う時の説明も書いてるけどそこまでしなくてもよくね？
+ Ch3 ここから実践．まずは画像認識界のFizzBuzzことMNISTをやる．その後はちょっとしたCNNでCIFAR-10の分類とか
+ Ch4 GANで画像生成．MNISTとかCIFARぽい画像を吐いてくれるニューラルネットを作ったり
+ Ch5 word2vecとか(まだよんでない)
+ Ch6 RNNの話．LSTMで極性分析したり，GRUで品詞の推定したり
+ Ch7 KerasのFunctional APIの使い方と，autoencoder, memory network, deep dream等，ちょっと応用的なニューラルネットの話
+ Ch8 DQNとか．強化学習をニューラルネットでやろうっていう話

感想

+ ほぼKerasの使い方に特化
+ 基本的なニューラルネットの説明(CNNとかLSTMとか)
+ ただし，数学的な説明はあまりない．Kerasの使い方に寄ってる．
+ 読みながらちょこちょこ手を動かすだけでCIFAR-10使って犬猫判別とか画像生成とかできるので学習意欲は湧きやすい
+ 画像認識やったこと無い人も読み始めて一時間後くらいには手書き文字認識できるようになってる．
+ ディープラーニングがどんなものかなんとなくわかるけど手を動かしたことはないみたいな人にはよさそう
+ ただやっぱりKerasの使い方に寄り過ぎてる感が否めないので，もう少し基本的な話については『詳解ディープラーニング』とか読んだほうが良さそう．
+ 実践的な例はこっちのほうが多いので，やってて楽しい
    - 犬猫判別とか
    - GANで画像生成とか
    - 文書の極性分析とか
    - Deep Dreamでキモい画像生成したりとか
+ それっぽいデータを元にそれっぽい結果が出て来るのを自分の目で確認できるのはいい
+ コードが見づらい．インデントがぶっ壊れてる．Pythonなので当然書いてある打ち込むとインデントが死んでるので動かない．
+ 終始Kerasの使い方の説明なので，適切なモデルを選ぶ方法とかはあまり学べない．だれか教えて．

<div class="amazlet-box" style="margin-bottom:0px;"><div class="amazlet-image" style="float:left;margin:0px 12px 1px 0px;"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/B072JC21DH/cloudear-22/ref=nosim/" name="amazletlink" target="_blank"><img src="https://images-fe.ssl-images-amazon.com/images/I/51wQTL5C0uL._SL160_.jpg" alt="詳解 ディープラーニング　TensorFlow・Kerasによる時系列データ処理" style="border: none;" /></a></div><div class="amazlet-info" style="line-height:120%; margin-bottom: 10px"><div class="amazlet-name" style="margin-bottom:10px;line-height:120%"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/B072JC21DH/cloudear-22/ref=nosim/" name="amazletlink" target="_blank">詳解 ディープラーニング　TensorFlow・Kerasによる時系列データ処理</a><div class="amazlet-powered-date" style="font-size:80%;margin-top:5px;line-height:120%">posted with <a href="http://www.amazlet.com/" title="amazlet" target="_blank">amazlet</a> at 18.01.21</div></div><div class="amazlet-detail">マイナビ出版 (2017-05-30)<br />売り上げランキング: 3,561<br /></div><div class="amazlet-sub-info" style="float: left;"><div class="amazlet-link" style="margin-top: 5px"><a href="http://www.amazon.co.jp/exec/obidos/ASIN/B072JC21DH/cloudear-22/ref=nosim/" name="amazletlink" target="_blank">Amazon.co.jpで詳細を見る</a></div></div></div><div class="amazlet-footer" style="clear: left"></div></div>

