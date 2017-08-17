---
layout: post
title: GaucheでEmacsのnadviceみたいなことができるやつを作った
tags:
- Gauche
- Emacs
---

## nadviceとは?

Emacsでアスペクト指向プログラミング的なことをできるようにするためのライブラリです。

アスペクト指向というのは聞き慣れない言葉かもしれませんが、nadviceを使うだけなら難しくはありません。簡単に言うと、任意の関数の実行前後に好きな処理を挟むことができるライブラリです。

```elisp
(defun hello-world ()
  (print "Hello, world!"))

(defun hello-world-before ()
  (print "before"))

;; hello-world を実行するときは、その前に hello-world-before を実行するようにする
(advice-add 'hello-world :before 'hello-world-before)

(hello-world)
;; "before"
;; "Hello, world!"
```

詳しい説明は以下のサイトを見るといいかもしれません。

[【関数再定義革命】Emacs 24.4ではdefadviceは時代遅れ！nadvice.elによる洗練された新しいアドバイス定義方法！！](http://emacs.rubikitch.com/nadvice/)

この機能を使うことで、例えばデバッグ中に怪しい関数の引数や返り値のログを吐かせたり、テスト中に呼ばれたくない関数を一時的にモックで差し替えたりといったことができるので結構便利です。

今回は、Gaucheで似たようなことをするライブラリを作りました。

## ソースコード

これです。もう少し細かいところを整備したらGithubに上げるかもしれません。

*advisor.scm*
```scheme
;; -*- coding: utf-8 -*-

(define-module advisor
  (use util.match)
  (export
   <advisable>
   make-advisable
   advice-add!
   %advice-add!
   advice-remove!
   %advice-remove!
   ))
(select-module advisor)

(define-class <advisable> ()
  (;; 元となる手続きもしくは callable
   [callable :init-keyword :callable]
   ;; callable の実行の周辺で呼ばれる手続きのリスト。
   [advices :init-value ()]
   ))

(define (make-advisable callable)
  (make <advisable> :callable callable))

;; Macro: convert-to-advisable-maybe! callable
;; callable が <advisable> のインスタンスでなかった場合、等価な((callable args ...) の返す
;; 値が等しい) <advisable> のインスタンスで callable の束縛を上書きする。
;; callable が <advisable> のインスタンスであった場合、何もしない。
(define-syntax convert-to-advisable-maybe!
  (syntax-rules ()
    [(_ callable)
     (when (not (is-a? callable <advisable>))
       (set! callable (make-advisable callable)))]
    ))

;; Macro: advice-add! advisable timing advice
;; advisable が <advisable> のインスタンスでなければ <advisable> のインスタンスに変換し、
;; %advice-add! により advice を追加する
(define-syntax advice-add!
  (syntax-rules ()
    [(_ advisable timing advice)
     (begin
       (convert-to-advisable-maybe! advisable)
       (%advice-add! advisable timing advice))]
    ))

;; Function: %advice-add! advisable timing advice
;; advisable の実行時に、指定した timing で手続き advice を実行する。
;; advice が受け取る引数、及び返すべき値は timing によって異なる。
;; timing は以下の値を取ることができる。
;;   + :around
;;        advisable の実行前に advice が呼ばれる。 advisable に渡された引数を
;;        args ... とすると、 advice には引数 advisable args ... が渡される。
;;        advice が rest-cont を実行しなかった場合、続きの処理は呼ばれない。
;;        また、 advice の返す値を全体の戻り値とする。
;;   + :before
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値は捨てられる。
;;   + :after
;;        advisable の実行後に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値は捨てられ、 advisable の戻り値が全体の戻り値となる。
;;   + :override
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値がそのまま返される。
;;   + :filter-args
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に返す。
;;        advice が(多値として)返した値を advisable の新たな引数にする。
;;   + :filter-return
;;        advisable の実行後に、 advisable が(多値として)返した値を引数に advice が呼ばれる。
;;        advice の戻り値がそのまま返される。
;;   + :before-if
;;        advisable の実行前に、 advisable に渡された引数をそのまま advice に渡す。
;;        advice の戻り値が真の値であった場合、同じ引数で advisable が呼ばれる。
;;        advice のもどちりが義の値であった場合、 advisable は呼ばれず、即座に #f が返される。
(define (%advice-add! advisable timing advice)
  (case timing
    [(:around :before :after :override :filter-args :filter-return :before-if)
     (set! (~ advisable 'advices) `((,timing . ,advice) . ,(~ advisable 'advices)))]
    [else (errorf "invalid timing: ~s" timing)]
    ))

;; Function: make-adviced-function proc timing advice return
;; proc をラップし、呼び出し時に timing で指定されるタイミングで advice を実行するような
;; 手続きを返す。
;; return は強制的にすべての手続きを終了させたい場合に呼ぶ継続。
;; timing と advice についての詳細は advice-add! を参照。
(define (make-adviced-function proc timing advice return)
  (case timing
    [(:around) (lambda args
                 (apply advice proc args))]
    [(:before) (lambda args
                 (apply advice args)
                 (apply proc args))]
    [(:after) (lambda args
                (let ([result (apply proc args)])
                  (apply advice args)
                  result))]
    [(:override) advice]
    [(:filter-args) (lambda args
                      (call-with-values (cut apply advice args) proc))]
    [(:filter-return) (lambda args
                        (call-with-values (cut apply proc args) advice))]
    [(:before-if) (lambda args
                    (if (apply advice args)
                        (apply proc args)
                        (return #f)))]
    [else (errorf "invalid timing: ~s" timing)]
    ))

;; Macro: advice-remove! advisable advice
;; %advice-remove! を用い、 advisable から advice を削除する。
;; advisable に一つも advice が残らなかった場合、 advisable をその callable で置き換える。
(define-syntax advice-remove!
  (syntax-rules ()
    [(_ advisable advice)
     (begin
       (%advice-remove! advisable advice)
       (when (null? (~ advisable 'advices))
         (set! advisable (~ advisable 'callable))))]
    ))

;; Function: %advice-remove! advisable advice
;; advisable から advice と eq? の意味で等しい advice をすべて削除する。
;; 等しい advice が存在しない場合は何もしない。
(define (%advice-remove! advisable advice)
  (set! (~ advisable 'advices)
        (filter (lambda (timing-and-advice)
                  (not (eq? advice (cdr timing-and-advice))))
                (~ advisable 'advices))))

;; Method: object-apply advisable . args
(define-method object-apply ([advisable <advisable>] . args)
  (let/cc return
    (define proc
      (let loop ([rest-cont (~ advisable 'callable)]
                 [advices (~ advisable 'advices)])
        (match advices
          [() rest-cont]
          [((timing . advice) . rest)
           (loop (make-adviced-function rest-cont timing advice return) rest)])
        ))
    (apply proc args)
    ))
```

## 使い方

現在使用可能はポイントカット(関数を呼び出すタイミング)は、`:around`, `:before`, `:after`, `:override`, `:filter-args`, `:filter-return`, `:before-if`の7種類です。

それぞれ、nadviceの`:around`, `:before`, `:after`, `:override`, `:filter-args`, `:filter-return`, `:before-while`に対応するはずです(実のところnadviceをそんなに詳しく知っているわけではないので本当に対応しているかはよくわかりません)。

```scheme
(use advisor)

;; 元の関数
(define (add x y)
  (format #t "add called x=~s y=~s\n" x y)
  (+ x y))

add
;; #<closure (add x y)>

(add 3 2)
;; add called x=3 y=2
;; 5



;; 実行前に処理を挟む
(define (add-before x y)
  (format #t ":before x=~s y=~s\n" x y))
(advice-add! add :before add-before)

;; advice-add! を呼ぶと、 add は元の手続きをラップした <advisable> のインスタンスで
;; 置き換えられる。
add
;; #<<advisable> 0x10160d5e0>

;; add の呼び出し前に add-before が呼ばれている
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; 5



;; 呼び出し後に処理を挟む
(define (add-after x y)
  (format #t ":after x=~s y=~s\n" x y))
(advice-add! add :after add-after)
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :after x=3 y=2
;; 5



;; 別な関数で処理を置き換える
(define (add-override x y)
  (format #t ":override x=~s y=~s\n" x y)
  (* x y))
(advice-add! add :override add-override)

;; add 自体は呼ばれず、代わりに add-override が呼ばれるようになった
(add 3 2)
;; :before x=3 y=2
;; :override x=3 y=2
;; :after x=3 y=2
;; 6



;; 追加した advice を削除する
(advice-remove! add add-override)

;; add-override が削除され、元通り add が呼ばれるようになった
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :after x=3 y=2
;; 5



;; 引数を加工する
(define (add-filter-args x y)
  (format #t ":filter-args x=~s y=~s\n" x y)
  (values (* x 2) (* y 2)))
(advice-add! add :filter-args add-filter-args)

;; 引数がすべて2倍されたため、 6 + 4 = 10 が返ってくるようになった
(add 3 2)
;; :before x=3 y=2
;; :filter-args x=3 y=2
;; add called x=6 y=4
;; :after x=3 y=2
;; 10

(advice-remove! add add-filter-args)
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :after x=3 y=2
;; 5



;; 戻り値を加工する
(define (add-filter-return ret)
  (format #t ":filter-return ret=~s\n" ret)
  (+ ret 10))
(advice-add! add :filter-return add-filter-return)

;; 戻り値に10が足されるようになり、15が返ってくるようになった
(add 3 2)
;; :before x=3 y=2
;; add called x=3 y=2
;; :filter-return ret=5
;; :after x=3 y=2
;; 15



;; 元の関数を実行するかどうかを決める
(define (add-before-if x y)
  (format #t ":before-if x=~s y=~s\n" x y)
  (even? x))
(advice-add! add :before-if add-before-if)

;; 第一引数が偶数のときのみ add が実行されるようになった
(add 3 2)
;; :before x=3 y=2
;; :before-if x=3 y=2
;; #f

(add 4 2)
;; :before x=4 y=2
;; :before-if x=4 y=2
;; add called x=4 y=2
;; :filter-return ret=6
;; :after x=4 y=2
;; 16



;; advice をすべて削除
(advice-remove! add add-before)
(advice-remove! add add-after)
(advice-remove! add add-filter-return)
(advice-remove! add add-before-if)

;; すべての advice が削除されると add は元の手続きに戻る
add
;; #<closure (add x y)>
```

## 問題点
現状の問題点として、例えば複数の`advice`を同じポイントカットに追加したときにどの順番で呼ぶべきかとか、`:before`と`:before-if`はどちらを先に呼ぶべきかとか、`:before-if`で実行されない判定がされたときに`:after`は呼ぶべきなのか呼ばないべきなのかとか、そういった所をうまいことやる方法が決まっていません。そこらへんが決まり次第、Githubにでも上げようかなと思っています。

アスペクト指向とnadviceに詳しい方助言をください。

