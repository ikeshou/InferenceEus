#|
組み込み大域変数、大域関数に型をつけるためのモジュール
その際必要になるため型変数のシンボル作成関数などもここで定義されている
|#

;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")


(require :fix "fixed_hash.l")
(require "env.l")
(require "type_util.l")
(require "type_class_tree.l")


(defparameter *sym-count* -1)


;;; 組み込み大域関数の集合 (fix:hash-table). set-ftype 時に関数名の表記が間違えていないか判定するために使用
(defparameter *all-funcs*
  (let ((ht (fix:make-hash-table :size 4096)))
    (dolist (pack (list-all-packages))
      (dolist (fn-sym (functions "" pack))
        (fix:sethash fn-sym ht t)))
    ht))


;;; easy-to-track gensym (|?a0|, |?a1|, and so on)
;;; 上界を時として伴う存在型を ? つきのシンボルで表す
(defun sym ()
  (incf *sym-count*)
  (intern (concatenate string "?a" (string *sym-count*))))

;;; easy-to-track gensym (|a0|, |a1|, and so on)
;;; 上界を時として伴う全称型を ^ つきのシンボルで表す
(defun univsym ()
  (incf *sym-count*)
  (intern (concatenate string "^a" (string *sym-count*))))


;;; 簡易的な型変数判定
;;; 型変数のシンボルの表記を変更するならこれも変更の必要あり
(defun typevarp (x)
  (and (symbolp x)
       (member (char (string x) 0) (list (char "?" 0) (char "^" 0)))
       (= (char (string x) 1) (char "a" 0))))



(defun build-varenv ()
  "
  存在する大域変数の型環境 (env, 辞書ではない) を構築して返す
  （組み込みの大域変数に限らない。これによりユーザー定義のクラスや大域変数などを型環境に追加できる）
  "
  (let ((varenv (make-env :size 512 :outer nil)))
    (dolist (pack (list-all-packages))
      (dolist (var-sym (variables "" pack))
        ; eval が重要。そうしないと全部シンボルになる
        (setenv var-sym varenv (class (eval var-sym)))))
    varenv))


;;; 1000個以上あるビルトイン関数の型情報の登録をラクに行うためのマクロ
;;; build-fenv 内で使用する省略記法
;;; !build-fenv のローカル変数 fenv をベタ書きしているので、ローカル変数の名前を変えた際には変更の必要あり!
;;;
;;; 以下のように列挙するだけで、クオートなしに登録可能
;;; e.g. (引数の型 a, 戻り値の型 r として)
;;;      (set-ftype {func} r0 a0 a1 (optional a2) (optional a3) (rest) ({key} a4) ({key} a5))
;;;
;; a0 -> a2 -> r なる関数型 (引数の型 a0, a1, 戻り値の型 r) は ((a0 a2) . r) と表記することにする
;; より詳細には、引数が (a:a0 b:a1 &optional c:a2 &rest d:cons &key e:a3) と型がつき、戻り値 f:a4 と型がついたとき、
;; 関数型を ((a0 a1 (optional . a2) (rest) (e . a3)) . a4) と表記する
(defmacro set-ftype (fn ret-type &rest arg-type)
  ; typo check
  (when (null (fix:gethash fn *all-funcs*)) (format t "unknown func symbol: ~s~%" fn)) 
  ; main
  `(setenv ',fn fenv ; !fenv は (build-fenv) 内のローカル変数名!
     (cons
        (mapcar #'(lambda (x) (cond 
                                ; (optional T) or (repeat T) or (someKey T)
                                ((and (consp x) (= (length x) 2))
                                  (cons (car x) (eval (car (cdr x)))))
                                ; (rest)
                                ((consp x) x)
                                ; class T
                                (t (eval x))))
                ',arg-type)
        ,ret-type)))


(defun build-fenv ()
  "
  組み込み大域関数の型環境 (env, 辞書ではない) を構築する
  構築した型環境、割り当てた型変数に対する ?a <: T 不等式の dot pair を返す 
  (ここで ?a :> T 不等式が立つことはない)

  Note:
    リファレンスに記載されている組み込みの関数およびマクロを以下に列挙している。
    ちょくちょくリファレンスの関数名称や戻り値の情報が正しくないので注意
    マクロだろうと区別せず、関数のように登録できるものは登録する
    関数でも引数を動的に解析すれば正確に戻り値型が着く場合はメインの推論関数に任せる。
    (オブジェクト生成の make-isntance とか、戻り値型指定のある coerce とか)
  
  Warn:
    極力型変数の制約をあつめて上界を求めるという性質上、引数に関しては型の集合として広くても（制約が立ちにくいだけで）問題ない
    (S or T を引数にとるが面倒なので Top にしちゃえ、など)
    しかし戻り値については (covariant なので) やってはだめ。本来ならエラーを起こさないものを型エラーとして誤検出してしまう。
    上界が求められなくなるので、型変数をおいてそれに上界を設定するという形をとる。
  "
  (let ((type-ineq-lt nil)
        (fenv (make-env :size 2048 :outer nil)))  ; !fenv の名称の変更時はマクロも変更する!
  ;;;;;;;;;;;
  ;;; eus ;;;
  ;;;;;;;;;;;
  ;;; 述語  ref:p.21
  (set-ftype eq bool object object)
  (set-ftype eql bool object object)
  (set-ftype equal bool object object)
  (set-ftype superequal bool object object)
  (set-ftype null bool object)
  (set-ftype not bool object)
  (set-ftype atom bool object)
  (set-ftype every bool compiled-code seq)
  (set-ftype some bool compiled-code seq)
  (set-ftype functionp bool object)
  (set-ftype compiled-function-p bool object)

  ;;; クラスとメソッド  ref:p.23
  ;; defclass
  ;; defmethod
  ;; defclassmethod
  (set-ftype classp bool object)
  (set-ftype subclassp bool metaclass metaclass)
  (set-ftype vector-class-p bool object)
  (set-ftype delete-method cons object)    ; なぜか成功時も失敗時も(7942 79)といったconsが返る
  (set-ftype class-hierarchy cons metaclass)
  (set-ftype system::list-all-classes cons nil-class)
  (set-ftype system::find-method cons metaclass object)  ; find-method と同じ
  (set-ftype system::method-cache cons (optional object))

  ;;; メッセージ送信  ref:p.24
  ;; send
  ;; send-message
  ;; send*
  (set-ftype send-all cons cons symbol (rest))  ; 関数。receiver の list を受け取りそれぞれに send して結果を list にする
  ;; send-super
  ;; send-super*

  ;;; インスタンス管理  ref:p.25
  ;; instantiate
  ;; instance
  ;; make-instance
  ;; 全称型
  ;; (set-ftype copy-object object object)
  (let ((a (univsym)))
    (setenv 'copy-object fenv
      (cons (list a) a)))
  ;; become
  ;; replace-object
  (set-ftype class metaclass object)
  (set-ftype derivedp bool object metaclass)
  (set-ftype slot any-class object metaclass object)
  (set-ftype setslot any-class object metaclass object object)

  ;;; 基本クラス  ref:p.26
  (set-ftype find-method cons metaclass object)    ; 成功時は(#<metaclass #X561a825ef9b8 object> (:slots #<compiled-code #X561a825b25d0>))のようなconsが、失敗時はnilが返る。第二引数はなんでも受け付ける

  ;;; 比較演算  ref:p.29
  (set-ftype numberp bool object)
  (set-ftype integerp bool object)
  (set-ftype floatp bool object)
  (set-ftype zerop bool number)
  (set-ftype plusp bool number)
  (set-ftype minusp bool number)
  (set-ftype oddp bool int)
  (set-ftype evenp bool int)
  (set-ftype /= bool number number (repeat number))
  (set-ftype = bool number number (repeat number))
  (set-ftype > bool number number (repeat number))
  (set-ftype < bool number number (repeat number))
  (set-ftype >= bool number number (repeat number))
  (set-ftype <= bool number number (repeat number))

  ;;; 整数とビット毎の操作関数 ref:p.30
  ;;; 戻り値の型 number としてはだめ、戻り値の型が広すぎて本来なら通るはずの型エラーを起こす
  ;; 戻り値の型を number で抑える
  ;; (set-ftype mod number number number)
  (let ((a (sym)))
    (setenv 'mod fenv
      (cons `(,number ,number) a))
    (push-cons-al (list a number) type-ineq-lt))
  (set-ftype 1- int int)
  (set-ftype 1+ int int)
  (set-ftype logand int (repeat int)) ; 引数 0 つでも OK
  (set-ftype logior int (repeat int))
  (set-ftype logxor int (repeat int))
  (set-ftype logeqv int (repeat int))
  (set-ftype lognand int (repeat int))
  (set-ftype lognor int (repeat int))
  (set-ftype lognot int int)
  (set-ftype logtest bool int int)
  (set-ftype logbitp bool int int)
  (set-ftype ash int int int)
  (set-ftype ldb int int int int)
  (set-ftype dpb int int int int int)

  ;;; 一般数値関数  ref:p.31
  ;; 戻り値を number で抑える
  ;; (set-ftype + number number (repeat number))
  (let ((a (sym)))
    (setenv '+ fenv
      (cons `(,number (repeat . ,number)) a))
    (push-cons-al (list a number) type-ineq-lt))
  ;; (set-ftype - number number (repeat number))
  (let ((a (sym)))
    (setenv '- fenv
      (cons `(,number (repeat . ,number)) a))
    (push-cons-al (list a number) type-ineq-lt))
  ;; (set-ftype * number number (repeat number))
  (let ((a (sym)))
    (setenv '* fenv
      (cons `(,number (repeat . ,number)) a))
    (push-cons-al (list a number) type-ineq-lt))
  ;; (set-ftype / number number number (repeat number))
  (let ((a (sym)))
    (setenv '/ fenv
      (cons `(,number ,number (repeat . ,number)) a))
    (push-cons-al (list a number) type-ineq-lt))
  ;; 上界 number を持つ全称型
  ;; (set-ftype abs number number)
  (let ((a (univsym)))
    (setenv 'abs fenv
      (cons (list a) a))
    (push-cons-al (list a number) type-ineq-lt))
  (set-ftype round int number)
  (set-ftype floor int number)
  (set-ftype ceiling int number)
  (set-ftype truncate int number)
  (set-ftype float float number)
  ;; 戻り値を number で抑える
  ;; (set-ftype max number (repeat number))
  (let ((a (sym)))
    (setenv 'max fenv
      (cons `((repeat . ,number)) a))
    (push-cons-al (list a number) type-ineq-lt))
  ;; (set-ftype min number (repeat number))
  (let ((a (sym)))
    (setenv 'min fenv
      (cons `((repeat . ,number)) a))
    (push-cons-al (list a number) type-ineq-lt))
  (set-ftype make-random-state integer-vector (optional integer-vector))
  ;; 上界 number を持つ全称型
  ;; (set-ftype random number number (optional integer-vector))
  (let ((a (univsym)))
    (setenv 'abs fenv
      (cons `(,a (optional . ,integer-vector)) a))
    (push-cons-al (list a number) type-ineq-lt))
  ;; incf
  ;; decf
  (set-ftype reduce any-class compiled-code seq)
  (set-ftype rad2deg float number)
  (set-ftype deg2rad float number)

  ;;; 基本関数  ref:p.33
  (set-ftype sin float number)
  (set-ftype cos float number)
  (set-ftype tan float number)
  (set-ftype sinh float number)
  (set-ftype cosh float number)
  (set-ftype tanh float number)
  (set-ftype asin float number)
  (set-ftype acos float number)
  (set-ftype atan float number)
  (set-ftype asinh float number)
  (set-ftype acosh float number)
  (set-ftype atanh float number)
  (set-ftype sqrt float number)
  (set-ftype log float number)
  (set-ftype exp float number)
  ;; 戻り値を number で抑える
  ;; (set-ftype expt number number number)
  (let ((a (sym)))
    (setenv 'expt fenv
      (cons `(,number ,number) a))
    (push-cons-al (list a number) type-ineq-lt))

  ;;; シンボル  ref:p.35
  (set-ftype symbolp bool object)
  (set-ftype symbol-value any-class symbol)
  (set-ftype symbol-function compiled-code symbol)
  (set-ftype symbol-package package symbol)
  (set-ftype symbol-name string symbol)
  (set-ftype symbol-plist cons symbol)
  (set-ftype boundp bool symbol)
  (set-ftype fboundp bool symbol)
  (set-ftype makunbound bool symbol)
  (set-ftype get any-class symbol object)
  ;; 全称型
  ;; (set-ftype putprop object symbol object object) ; (putprop sym val attr) で sym の plist に (attr . val) が追加され val が返る
  (let ((a (univsym)))
    (setenv 'putprop fenv
      (cons `(,symbol ,a ,object) a)))
  (set-ftype remprop cons symbol object)  ; 残った plist が返る
  ;; setq
  ;; set
  ;; defun
  ;; defmacro
  ;; defvar
  ;; defparameter
  ;; defconstant
  (set-ftype keywordp bool object)
  (set-ftype constantp bool object)
  (set-ftype documentation string symbol (optional object))
  (set-ftype gensym symbol (optional object))
  (set-ftype gentemp symbol (optional string) (optional packlike))

  ;;; パッケージ  ref:p.37
  (set-ftype find-symbol symbol string (optional packlike))
  (set-ftype make-symbol symbol string)
  (set-ftype intern symbol string (optional packlike) (optional symbol))
  (set-ftype list-all-packages cons nil-class)
  (set-ftype find-package package packlike)
  (set-ftype make-package package symlike (nicknames symlike))
  (set-ftype in-package package packlike (nicknames symlike))
  (set-ftype package-name string packlike)
  (set-ftype package-nicknames cons packlike)
  (set-ftype rename-package package packlike symlike (optional symlike))
  (set-ftype package-use-list cons packlike)
  (set-ftype packagep bool object)
  (set-ftype use-package package packlike (optional packlike))
  (set-ftype unuse-package package packlike (optional packlike))
  (set-ftype shadow bool symbol (optional packlike))
  (set-ftype export bool symbol (optional packlike))
  ;; unexport などない
  ;; (set-ftype unexport symbol symbol (optional packlike))
  (set-ftype import bool symbol (optional packlike))
  ;; do-symbols
  ;; do-external-symbols
  ;; do-all-symbols

  ;;; 一般列  ref:p.40
  (set-ftype elt any-class seq int)
  (set-ftype length int seq)
  (set-ftype subseq seq seq int (optional int))
  ;; 上界 seq を持つ全称型
  ;; (set-ftype copy-seq seq seq)
  (let ((a (univsym)))
    (setenv 'copy-seq fenv
      (cons (list a) a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype reverse seq seq)
  (let ((a (univsym)))
    (setenv 'reverse fenv
      (cons (list a) a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype nreverse seq seq)
  (let ((a (univsym)))
    (setenv 'nreverse fenv
      (cons (list a) a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; concatenate
  ;; coerce
  ;; map
  ;; 上界 seq を持つ全称型
  ;; (set-ftype fill seq seq object (start int) (end int))
  (let ((a (univsym)))
    (setenv 'fill fenv
      (cons `(,a ,object (start . ,int) (end . ,int)) a))
    (push-cons-al (list a seq) type-ineq-lt))  
  ;; (set-ftype replace seq seq (start1 int) (end1 int) (start2 int) (end2 int))
  (let ((a (univsym)))
    (setenv 'replace fenv
      (cons `(,a ,object (start1 . ,int) (end1 . ,int) (start2 . ,int) (end2 . ,int)) a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype sort seq seq compiled-code (optional compiled-code))
  (let ((a (univsym)))
    (setenv 'sort fenv
      (cons `(,a ,compiled-code (optional . ,compiled-code)) a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; merge
  (set-ftype merge-list cons cons cons compiled-code compiled-code)    ; こちらは key の関数 (absとか)が optional ではない
  (set-ftype position int object seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code) (count int))
  (set-ftype position-if int compiled-code seq (start int) (end int) (key compiled-code))
  (set-ftype position-if-not int compiled-code seq (start int) (end int) (key compiled-code))
  (set-ftype find any-class object seq (start int) (end int) (key compiled-code) (count int))
  (set-ftype find-if any-class compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (set-ftype find-if-not any-class compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (set-ftype count int object seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code))
  (set-ftype count-if int compiled-code seq (start int) (end int) (key compiled-code))
  (set-ftype count-if-not int compiled-code seq (start int) (end int) (key compiled-code))
  ;; 上界 seq を持つ全称型
  ;; (set-ftype remove seq object seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'remove fenv
      (cons `(,object ,a (start . ,int) (end . ,int) (test . ,compiled-code) (test-not . ,compiled-code) (key . ,compiled-code) (count . ,int))
              a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype remove-if seq compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'remove-if fenv
      (cons `(,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
              a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype remove-if-not seq compiled-code seq (start int) (end int) (key compiled-code) (count int)) 
  (let ((a (univsym)))
    (setenv 'remove-if-not fenv
      (cons `(,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
              a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype remove-duplicates seq seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'remove-duplicates fenv
      (cons `(,a (start . ,int) (end . ,int) (test . ,compiled-code) (test-not . ,compiled-code) (key . ,compiled-code) (count . ,int))
              a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype delete seq object seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'delete fenv
      (cons `(,object ,a (start . ,int) (end . ,int) (test . ,compiled-code) (test-not . ,compiled-code) (key . ,compiled-code) (count . ,int))
              a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype delete-if seq compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'delete-if fenv
      (cons `(,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype delete-if-not seq compiled-code seq (start int) (end int) (key compiled-code) (count int)) 
  (let ((a (univsym)))
    (setenv 'delete-if-not fenv
      (cons `(,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype substitute seq object object seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code) (count int)) 
  (let ((a (univsym)))
    (setenv 'substitute fenv
      (cons `(,object ,object ,a (start . ,int) (end . ,int) (test . ,compiled-code) (test-not . ,compiled-code) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype substitute-if seq object compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'substitute-if fenv
      (cons `(,object ,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))  
  ;; (set-ftype substitute-if-not seq object compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'substitute-if-not fenv
      (cons `(,object ,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype nsubstitute seq object object seq (start int) (end int) (test compiled-code) (test-not compiled-code) (key compiled-code) (count int)) 
  (let ((a (univsym)))
    (setenv 'nsubstitute fenv
      (cons `(,object ,object ,a (start . ,int) (end . ,int) (test . ,compiled-code) (test-not . ,compiled-code) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))
  ;; (set-ftype nsubstitute-if seq object compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'nsubstitute-if fenv
      (cons `(,object ,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))  
  ;; (set-ftype nsubstitute-if-not seq object compiled-code seq (start int) (end int) (key compiled-code) (count int))
  (let ((a (univsym)))
    (setenv 'nsubstitute-if-not fenv
      (cons `(,object ,compiled-code ,a (start . ,int) (end . ,int) (key . ,compiled-code) (count . ,int))
            a))
    (push-cons-al (list a seq) type-ineq-lt))

  ;;; リスト  ref:p.44
  (set-ftype listp bool object)
  (set-ftype consp bool object)
  (set-ftype car any-class cons)
  (set-ftype cdr cons cons)
  (set-ftype cadr any-class cons)
  (set-ftype cddr cons cons)
  (set-ftype caar any-class cons)
  (set-ftype cdar cons cons)
  (set-ftype caddr any-class cons)
  (set-ftype caadr any-class cons)
  (set-ftype cadar any-class cons)
  (set-ftype caaar any-class cons)
  (set-ftype cdadr cons cons)
  (set-ftype cdaar cons cons)
  (set-ftype cdddr cons cons)
  (set-ftype cddar cons cons)
  (set-ftype first any-class cons)
  (set-ftype nth any-class int cons)
  (set-ftype nthcdr cons int cons)
  (set-ftype last cons cons)
  (set-ftype butlast cons cons (optional int))
  (set-ftype cons cons object object)
  (set-ftype list cons (repeat object))
  (set-ftype list* cons (repeat object))
  (set-ftype list-length int cons)
  (set-ftype make-list cons int (initial-element object))
  (set-ftype rplaca cons cons object)
  (set-ftype rplacd cons cons object)
  (set-ftype memq bool object cons)
  (set-ftype member cons object cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype assq cons object cons)
  (set-ftype assoc cons object cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype rassoc cons object cons)
  (set-ftype pairlis cons cons cons (optional cons))
  (set-ftype acons cons object object cons)
  (set-ftype append cons (repeat cons))
  (set-ftype nconc cons (repeat cons))
  (set-ftype subst cons object object cons)
  (set-ftype flatten cons cons)
  ;; push
  ;; pop
  ;; pushnew
  (set-ftype adjoin cons object cons)
  (set-ftype union cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype subsetp bool cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype intersection cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype set-difference cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype set-exclusive-or cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
  (set-ftype list-insert cons object int cons)
  (set-ftype copy-tree cons cons)
  (set-ftype mapc cons compiled-code (repeat cons))
  (set-ftype mapcar cons compiled-code (repeat cons))
  (set-ftype mapcan cons compiled-code (repeat cons))

  ;;; ベクトルと行列  ref:p.48
  (set-ftype vectorp bool object)
  (set-ftype vector vector (repeat object))
  ;; 戻り値を veclike でおさえる
  ;; (set-ftype make-array object object (element-type metaclass)
  ;;                                     (initial-contents object)
  ;;                                     (initial-element object)
  ;;                                     (fill-pointer object)
  ;;                                     (displaced-to object)
  ;;                                     (displaced-index-offset int)
  ;;                                     (adjustable bool))
  (let ((a (sym)))
    (setenv 'make-array fenv
      (cons `(,dim (element-type . ,metaclass)
                   (initial-contents . ,object)
                   (initial-element . ,object)
                   (fill-pointer . ,object)
                   (displaced-to . ,object)
                   (displaced-index-offset . ,int)
                   (adjustable . ,bool))
            a))
    (push-cons-al (list a veclike) type-ineq-lt))
  (set-ftype svref any-class vector int)
  (set-ftype aref any-class veclike (repeat int))
  (set-ftype vector-push array object array)
  (set-ftype vector-push-extend array object array)
  (set-ftype arrayp bool object)
  (set-ftype array-total-size int array)
  (set-ftype fill-pointer any-class array)
  (set-ftype array-rank int array)
  (set-ftype array-dimensions cons array)
  (set-ftype array-dimension int array int)
  (set-ftype bit int bit-vector int)
  (set-ftype bit-vector-p bool object)  ; リファレンスに書いてないけどしっかりある
  (set-ftype bit-and bit-vector bit-vector bit-vector (optional bit-vector))
  (set-ftype bit-ior bit-vector bit-vector bit-vector (optional bit-vector))
  (set-ftype bit-xor bit-vector bit-vector bit-vector (optional bit-vector))
  (set-ftype bit-eqv bit-vector bit-vector bit-vector (optional bit-vector))
  (set-ftype bit-nand bit-vector bit-vector bit-vector (optional bit-vector))
  (set-ftype bit-nor bit-vector bit-vector bit-vector (optional bit-vector))
  (set-ftype bit-not bit-vector bit-vector bit-vector (optional bit-vector))

  ;;; 文字と文字列  ref:p.50
  (set-ftype digit-char-p bool object)
  (set-ftype alpha-char-p bool object)
  (set-ftype upper-case-p symbol object)
  (set-ftype lower-case-p symbol object)
  ;; リファレンスの alphanumeric-p はまちがい
  (set-ftype alphanumericp symbol object)
  (set-ftype char-upcase int int)
  (set-ftype char-downcase int int)
  (set-ftype char int string int)
  (set-ftype schar int string int)
  (set-ftype stringp bool object)
  (set-ftype string-upcase string string (start int) (end int))
  (set-ftype string-downcase string string (start int) (end int))
  (set-ftype nstring-upcase string string (start int) (end int))
  (set-ftype nstring-downcase string string (start int) (end int))
  (set-ftype string= bool string string (start1 int) (end1 int) (start2 int) (end2 int))
  (set-ftype string-equal bool string string (start1 int) (end1 int) (start2 int) (end2 int))
  (set-ftype string string object)
  (set-ftype string< bool string string)
  (set-ftype string<= bool string string)
  (set-ftype string> bool string string)
  (set-ftype string>= bool string string)
  (set-ftype string-left-trim string string string)
  (set-ftype string-right-trim string string string)
  (set-ftype string-trim string string string)
  (set-ftype substringp bool string string)

  ;;; Foreign String  ref:p.52
  (set-ftype make-foreign-string foreign-string int int)    ; (unix:malloc int) returns int

  ;;; ハッシュテーブル  ref:p.53
  (set-ftype sxhash int object)
  (set-ftype make-hash-table hash-table (size int) (test compiled-code) (rehash-size int))
  (set-ftype gethash any-class object hash-table)
  (set-ftype remhash nil-class object hash-table) ; 成功しても nil を返す
  (set-ftype maphash cons compiled-code hash-table)
  (set-ftype hash-table-p bool object)

  ;;; ストリーム  ref:p.55
  (set-ftype streamp bool object)
  (set-ftype input-stream-p bool object)
  (set-ftype output-stream-p bool object)
  (set-ftype io-stream-p bool object)
  (set-ftype open stream pathlike (direction symbol) (if-exists symbol) (if-does-not-exist symbol) (permission int) (buffer-size int))
  (set-ftype close bool stream) ; 成功したら t
  (set-ftype make-string-input-stream stream string)
  (set-ftype make-string-output-stream stream int)
  (set-ftype get-output-stream-string string stream)
  (set-ftype make-broadcast-stream stream (repeat stream))

  ;;; リーダ  ref:p.57
  ;; 本当は any じゃなくす全てこういう上ななしの型変で指定した方いいいのだろうなあ
  (let ((a (sym)))
    (setenv 'read fenv
      (cons `((optional . ,stream) (optional . ,bool) (optional . ,object) (optional . ,bool)) a)))

  ;; (set-ftype read any-class (optional stream) (optional bool) (optional object) (optional bool))  ; eof-value はなんでも OK
  (set-ftype read-delimited-list any-class int (optional stream) (optional bool)) ; char は int
  (set-ftype read-line string (optional stream) (optional bool) (optional object))
  (set-ftype read-char int (optional stream) (optional bool) (optional object))
  (set-ftype read-from-string any-class string (optional bool) (optional object))
  (set-ftype unread-char int int (optional stream))
  (set-ftype peek-char int (optional stream) (optional bool) (optional object))
  (set-ftype y-or-n-p bool (optional string) (rest))
  (set-ftype yes-or-no-p bool (optional string) (rest)) ; リファレンスが間違えているはず
  ; readtable-p とリファレンスに間違えて記載されている
  (set-ftype readtablep bool object)
  (set-ftype copy-readtable readtable (optional readtable) (optional readtable))
  (set-ftype set-syntax-from-char int int int (optional readtable) (optional readtable))
  (set-ftype set-macro-character int int compiled-code (optional bool) (optional readtable))
  (set-ftype get-macro-character int int (optional readtable)) ; (get-macro-character 34 *readtable*) => 4504682096
  (set-ftype set-dispatch-macro-character int int int compiled-code (optional readtable))
  (set-ftype get-dispatch-macro-character int int int (optional readtable))

  ;;; プリンタ  ref:p.61
  (set-ftype print any-class object (optional stream))  ; 実は戻り値は引数と一致するけどそんな使い方しないので省略
  (set-ftype prin1 any-class object (optional stream))
  (set-ftype princ any-class object (optional stream))
  (set-ftype terpri nil-class (optional stream))
  (set-ftype finish-output nil-class (optional stream))
  (set-ftype princ-to-string string object (optional int))
  (set-ftype prin1-to-string string object (optional int))
  (set-ftype format string stream string (rest))
  (set-ftype pprint nil-class object (optional stream) (optional int) (optional int))
  ;; print-functions などない
  (set-ftype write-byte int int stream)
  (set-ftype write-word int int stream)
  (set-ftype write-long int int stream)
  (set-ftype spaces nil-class int (optional stream))
  ;; pf
  (set-ftype pp-method nil-class metaclass symbol (optional stream))
  (set-ftype tprint nil-class object cons (optional int) (optional int) (optional int))
  (set-ftype print-size int object)


  ;;; プロセス間通信とネットワーク  ref:p.63
  (set-ftype map-file foreign-string pathlike (direction symbol) (length int) (offset int) (share bool) (address int))
  (set-ftype make-msgq-input-stream file-stream int (optional int)) ; key も int
  (set-ftype make-msgq-output-stream file-stream int (optional int))
  (set-ftype make-socket-address socket-address (domain int) (host string) (port int))
  (set-ftype make-socket-port socket-port socket-address)
  (set-ftype make-server-socket-stream socket-stream socket-port (optional int))
  (set-ftype make-client-socket-stream socket-stream socket-address (optional int))
  (set-ftype open-server bool int compiled-code)
  (set-ftype connect-server stream string int)

  ;;; 非同期入出力  ref:p.66
  (set-ftype select-stream cons cons int)
  ;; def-async

  ;;; パス名  ref:p.67
  (set-ftype pathnamep bool object)
  (set-ftype pathname pathname pathlike)
  (set-ftype pathname-directory cons pathlike)
  (set-ftype pathname-name string pathlike)
  (set-ftype pathname-type string pathlike)
  (set-ftype make-pathname pathname (host object) (device object) (directory object) (name object) (type object) (version object)) 
  ;; merge-pathnames
  (set-ftype namestring string pathlike)
  (set-ftype parse-namestring pathname pathlike)
  (set-ftype truename pathname pathlike)

  ;;; ファイルシステムインターフェース  ref:p.68
  (set-ftype probe-file bool pathlike)
  (set-ftype file-size int pathlike)
  ;; directory-p などない
  (set-ftype find-executable pathname pathlike)
  (set-ftype file-write-date int pathlike)
  (set-ftype file-newer bool pathlike pathlike)
  (set-ftype object-file-p bool pathlike)
  (set-ftype directory cons (optional pathlike))
  (set-ftype dir nil-class (optional pathlike))    ; ls したときのような出力となるが、戻り値は nil

  ;;; 評価関数  ref:p.69
  ;; 全称型
  ;; (set-ftype identity object object)
  (let ((a (univsym)))
    (setenv 'identity fenv
      (cons (list a) a)))
  (set-ftype eval any-class object)
  ;; この二つはしっかりやるともっと情報を得られる
  ;; apply
  ;; funcall
  ;; quote
  ;; function
  (set-ftype evalhook any-class compiled-code object)
  (set-ftype eval-dynamic any-class symbol)
  (set-ftype macroexpand cons object)
  ;; eval-when
  ;; the
  ;; declare
  (set-ftype proclaim nil-class cons) ; form は cons S 式である必要があるっぽい
  (set-ftype warn any-class string (rest))
  (set-ftype error any-class string (rest)) ; 戻り値を使えるわけなし
  ;; install-error-handlerなどない

  ;;; 最上位レベルの対話  ref:p.72
  (set-ftype eustop nil-class (rest))
  ;; eussigなどない
  (set-ftype sigint-handler symbol object object)
  (set-ftype euserror any-class object object (rest))
  (set-ftype reset any-class nil-class)
  (set-ftype exit any-class (optional int)) ; termination code
  (set-ftype h nil-class nil-class)
  (set-ftype ! any-class (optional object)) ; optional int とリファレンスにはあるが再現できず
  (set-ftype new-history cons int)

  ;;; コンパイル  ref:p.74
  ;; euscomp (unix command)
  (set-ftype compile-file any-class pathlike (verbose bool) (optimize int) (c-optimize int) (safety int) (pic bool) (cc bool) (entry object))
  (set-ftype compile cons symbol)
  ;; compile-file-if-src-newer などない
  ;; (set-ftype compile-file-if-src-newer any-class pathlike (verbose bool) (optimize int) (c-optimize int) (safety int) (pic bool) (cc bool) (entry object))

  ;;; ロード
  (set-ftype load bool pathlike (verbose bool) (package package) (entry object) (symbol-input string) (symbol-output string) (ld-option string))
  (set-ftype load-files bool (repeat pathlike))
  (set-ftype provide symbol symbol)
  (set-ftype require bool symlike (optional pathlike))
  ;; system:binload
  ;; system:txtload

  ;;; デバッグ補助
  (set-ftype describe nil-class object (optional stream))
  (set-ftype describe-list nil-class cons (optional stream))
  (set-ftype inspect any-class object)
  (set-ftype more any-class object)
  (set-ftype break any-class (optional string))
  (set-ftype help nil-class symbol)
  (set-ftype apropos nil-class symbol)
  (set-ftype apropos-list cons symbol)
  (set-ftype constants cons (optional string) (optional packlike))
  (set-ftype variables cons (optional string) (optional packlike))
  (set-ftype functions cons (optional string) (optional packlike))
  ;; btrace
  ;; step-hook
  ;; step
  ;; trace
  ;; untrace
  (set-ftype timing nil-class int (rest)) ; macro だけど。表示するだけであたいは返さない
  (set-ftype time cons compiled-code) ; macro だけど
  (set-ftype sys:list-all-catchers cons nil-class)
  (set-ftype sys:list-all-instances cons metaclass)
  (set-ftype sys:list-all-bindings cons nil-class)
  (set-ftype sys:list-all-special-bindings cons nil-class)
  ;; dump-object
  ;; dump-structure
  ;; dump-loadable-structure
  ;; save
  (set-ftype lisp-implementation-type string nil-class)
  (set-ftype lisp-implementation-version string nil-class)


  ;;;;;;;;;;;;;;
  ;;; irteus ;;;
  ;;;;;;;;;;;;;;
  ;;; システム関数  ref:p.82
  (set-ftype sys:gc cons nil-class) ; 2 要素の cons が返る。実は引数は何を与えても無視されるが
  (set-ftype sys:gctime cons nil-class) ; 3 要素の (略)
  ;; sys:alloc
  ;; sys:newstack
  ;; sys:reclaim
  ;; sys:reclaim-tree
  ;; sys:btrace
  ;; sys:memory-report
  ;; sys:room
  ;; sys:address
  ;; sys:peek
  ;; sys:poke
  ;; sys:list-all-chunks
  ;; sys:object-size

  ;;; UNIX システムコール ref:p.85
  (set-ftype unix:ptimes cons nil-class)  ; 5 要素の cons が返る。実は引数は何を与えても無視されるが
  (set-ftype unix:runtime int nil-class)  ; 整数が返る (単位が 1/60 sec なので)
  (set-ftype unix:localtime vector nil-class) ; integer-vector が返るとリファレンスにはあるが vector が返る
  (set-ftype unix:asctime string vector)
  ;; unix:getpid
  ;; unix:getppid
  ;; unix:setpgrp
  ;; unix:getuid
  ;; unix:geteuid
  ;; unix:getgid
  ;; unix:getegid
  ;; unix:setuid
  ;; unix:setgid
  ;; unix:fork
  ;; unix:vfork
  ;; unix:exec
  ;; unix:wait
  (set-ftype unix::exit int int)  ; 終了状態は整数。リファレンスは uni:exit だが :: が正しい
  ;; unix:getpriority
  ;; unix:setpriority
  ;; unix:getusage
  ;; unix:system
  ;; unix:getenv
  ;; unix:putenv
  (set-ftype unix:sleep bool int) ; 整数時間停止、成功したら t を返す
  (set-ftype unix:usleep bool int)
  ;; unix:uread
  ;; unix:write
  ;; unix:fcntl
  ;; unix:ioctl
  ;; unix:ioctl_R
  ;; unix:ioctl_W
  ;; unix:ioctl_WR
  ;; unix:close
  ;; unix:dup
  ;; unix:pipe
  ;; unix:lseek
  ;; unix:link
  ;; unix:unlink
  ;; unix:mknod
  ;; unix:mkdir
  ;; unix:access
  ;; unix:stat
  ;; unix:chdir
  ;; unix:getwd
  ;; unix:chmod
  ;; unix:chown
  ;; unix:isatty
  ;; unix:msgget
  ;; unix:msgsnd
  ;; unix:msgrcv
  ;; unix:socket
  ;; unix:bind
  ;; unix:connect
  ;; unix:listen
  ;; unix:accept
  ;; unix:recvfrom
  ;; unix:sendto
  ;; unix:getservbyname
  ;; unix:gethostbyname
  ;; unix:syserrlist
  ;; unix:signal
  ;; unix:kill
  ;; unix:pause
  ;; unix:alarm
  ;; unix:ualarm
  ;; unix:getitimer
  ;; unix:setitimer
  ;; unix:select
  ;; unix:select-read-fd
  ;; unix:thr-self
  ;; unix:thr-getprio
  ;; unix:thr-setprio
  ;; unix:thr-getconcurrency
  ;; unix:thr-setconcurrency
  ;; unix:thr-create
  ;; unix:malloc
  ;; unix:free
  ;; unix:valloc
  ;; unix:mmap
  ;; unix:munmap
  ;; unix:vadvise
  ;; unix:tiocgetp
  ;; unix:tiocsetp
  ;; unix:tiocflush
  ;; unix:tiocgpgrp
  ;; unix:tiocoutq
  ;; unix:fionread
  ;; unix:tiocsetc
  ;; unix:tioclbis
  ;; unix:tioclbic
  ;; unix:tioclset
  ;; unix:tioclget
  ;; unix:tcseta
  ;; unix:tcsets
  ;; unix:tcsetsw
  ;; unix:tcsetsf
  ;; unix:tiocsetc
  ;; unix:tcsetaf
  ;; unix:tcsetaw
  ;; unix:tcgeta
  ;; unix:tcgets
  ;; unix:tcgetattr
  ;; unix:tcsetattr
  ;; dbm-open
  ;; dbm-store
  ;; dbm-fetch
  ;; cd
  ;; ez
  ;; piped-fork
  ;; rusage
  
  ;;; FFI ref:p.96
  ;; load-foreign
  ;; defforeign
  ;; defun-c-callable
  ;; pod-address
  (set-ftype array-entity vector array)
  ;; float2double, double2float などない
  ;; (set-ftype float2double float-vector float-vector)
  ;; (set-ftype double2float float-vector float-vector)
  
  ;;; VxWorks ref:p.101 (irteusgl にも入っていない謎の何か)
  ;; connect-vxw
  ;; vxw
  ;; defvxw
  
  ;;; マルチスレッド  ref:p.103
  ;; sys:free-threads
  ;; sys:thread
  ;; sys:thread-no-wait
  ;; sys:wait-thread
  ;; sys:plist
  ;; sys:make-mutex-lock
  ;; sys:mutex-lock
  ;; sys:mutex-unlock
  ;; sys:mutex
  ;; sys:make-cond
  ;; sys:cond-wait
  ;; sys:cond-signal
  ;; sys:make-semaphore
  ;; sys:sema-post
  ;; sys:sema-wait
  
  ;;; 幾何学関数  ref:p.111
  (set-ftype float-vector float-vector (repeat number))
  (set-ftype float-vector-p bool object)
  ; リファレンスにはないが追記
  (set-ftype integer-vector integer-vector (repeat int))
  (set-ftype integer-vector-p bool object)
  (set-ftype v+ float-vector float-vector float-vector (optional float-vector))
  (set-ftype v- float-vector float-vector (optional float-vector) (optional float-vector))  ; 第二引数も optional
  (set-ftype v. float-vector float-vector float-vector)
  (set-ftype v* float-vector float-vector float-vector (optional float-vector))
  (set-ftype v.* float-vector float-vector float-vector float-vector)
  (set-ftype v< bool float-vector float-vector)
  (set-ftype v> bool float-vector float-vector)
  (set-ftype vmin float-vector float-vector (repeat float-vector))  ; 引数に一つは必ず必要
  (set-ftype vmax float-vector float-vector (repeat float-vector))  
  (set-ftype minimal-box float cons float-vector float-vector (optional number))
  (set-ftype scale float-vector int float-vector (optional float-vector))
  (set-ftype norm float float-vector)
  (set-ftype norm2 float float-vector)
  (set-ftype normalize-vector float-vector float-vector)
  (set-ftype distance float float-vector float-vector)
  (set-ftype distance2 float float-vector float-vector)
  (set-ftype geometry::homo2normal float-vector float-vector) ; 要素が 4 -> 3
  (set-ftype geometry::homogenize float-vector float-vector)  ; 要素が 3 -> 4
  (set-ftype midpoint float-vector number float-vector float-vector (optional float-vector))
  (set-ftype rotate-vector float-vector float-vector number axis (optional float-vector))
  (set-ftype matrix mat seq (repeat seq))
  (set-ftype make-matrix mat int int (optional seq))
  (set-ftype matrixp bool object)
  (set-ftype matrix-row float-vector mat int)
  (set-ftype matrix-column float-vector mat int)
  (set-ftype m* mat mat mat (optional mat))
  (set-ftype transpose mat mat (optional mat))
  ;; unix-matrix などない
  ;; unix-matrix
  (set-ftype replace-matrix mat mat mat)
  (set-ftype scale-matrix mat number mat)
  (set-ftype copy-matrix mat mat)
  ; transform は (fvec, mat) or (mat, fvec)
  ; 引数型についての制約なので両方 fvec ということにしてよい
  ; 戻り値型は n*2 * 2*2 => n*2, n=2 なら mat になりうる
  ;          2*2 * 2*n => 2*n, n=2 なら mat になりうる
  ; ともに mat なら mat が返るが... 戻り値に関しては上界で抑える
  ; (set-ftype transform float-vector foat-vector float-vector)
  (let ((a (sym)))
    (setenv 'transform fenv
      (cons `(,float-vector ,float-vector) a))
    (push-cons-al (list a float-vector) type-ineq-lt))
  (set-ftype rotate-matrix mat mat number axis (optional object) (optional mat))
  (set-ftype rotation-matrix mat number axis (optional mat))
  (set-ftype rotation-angle cons mat)   ; float と flaot-vector の list が返る
  (set-ftype rpy-matrix mat number number number)
  (set-ftype rpy-angle cons mat)
  (set-ftype Euler-matrix mat number number number)
  (set-ftype Euler-angle cons mat)
  (set-ftype lu-decompose vector mat (optional mat))
  (set-ftype lu-solve mat vector vector (optional mat))
  (set-ftype lu-determinant float mat vector)
  (set-ftype simultaneous-equation float-vector mat float-vector)
  (set-ftype inverse-matrix mat mat)
  (set-ftype pseudo-inverse mat mat)
  (set-ftype coordinates-p bool object)
  (set-ftype make-coords coordinates 
    (pos float-vector) (rot mat) (rpy seq) (Euler seq) (angle number) 
    (axis axis) (4X4 mat) (coords coordinates) (name object))
  (set-ftype coords coordinates 
    (pos float-vector) (rot mat) (rpy seq) (Euler seq) (angle number) 
    (axis axis) (4X4 mat) (coords coordinates) (name object))
  (set-ftype make-cascoords cascaded-coords 
    (pos float-vector) (rot mat) (rpy seq) (Euler seq) (angle number) 
    (axis axis) (4X4 mat) (coords coordinates) (name object))
  (set-ftype cascoords cascaded-coords 
    (pos float-vector) (rot mat) (rpy seq) (Euler seq) (angle number) 
    (axis axis) (4X4 mat) (coords coordinates) (name object))
  (set-ftype transform-coords coordinates coordinates coordinates (optional coordinates))
  (set-ftype transform-coords coordinates (repeat coordinates))
  (set-ftype geometry::wrt float-vector coordinates float-vector)


  ;;;;;;;;;;;;;;;;
  ;;; irteusgl ;;;
  ;;;;;;;;;;;;;;;;
  ;;; 幾何学モデリング  ref:p.121
  (set-ftype vplus float-vector cons) ; 引数は float-vector の list
  (set-ftype vector-mean float-vector cons)
  (set-ftype triangle float float-vector float-vector float-vector (optional float-vector))
  (set-ftype triangle-normal float-vector float-vector float-vector float-vector)
  (set-ftype vector-angle float float-vector float-vector (optional float-vector))
  (set-ftype face-normal-vector float-vector cons)
  (set-ftype farthest float-vector float-vector cons)
  (set-ftype farthest-pair cons cons)
  (set-ftype maxindex int float-vector)
  (set-ftype random-vector float-vector (optional number))  ; range は int でも OK
  (set-ftype random-normalized-vector float-vector (optional number))
  (set-ftype random-vectors cons int number)
  (set-ftype geometry::line-intersection cons float-vector float-vector float-vector float-vector)
  ;; collinear-p などない
  ;; collinear-p
  (set-ftype find-coplanar-vertices float-vector float-vector float-vector float-vector cons)
  (set-ftype geometry::find-connecting-edge float-vector float-vector cons) ; vertex は fvec のことで、edge list は edge クラスのリスト
  (set-ftype geometry::make-vertex-edge-htab hash-table cons)
  (set-ftype left-points cons cons float-vector float-vector float-vector)
  (set-ftype right-points cons cons float-vector float-vector float-vector)
  ;; left-most-point などない
  ;; (set-ftype left-most-point float-vector cons float-vector float-vector float-vector)    
  ;; (set-ftype right-most-point float-vector cons float-vector float-vector float-vector)  
  (set-ftype eps= bool number number number)  ; int でも OK
  (set-ftype eps< bool number number number)
  (set-ftype eps<= bool number number number)
  (set-ftype eps> bool number number number)
  (set-ftype eps>= bool number number number)
  ;;; bounding-box 関連 ref:p.123
  (set-ftype make-bounding-box bounding-box cons (optional number))
  (set-ftype bounding-box-union bounding-box cons (optional number))
  (set-ftype bounding-box-intersection bounding-box cons (optional number))
  ;;; line, edge, plane, polygon, face, hole, body 関連 ref:p.125
  (set-ftype make-line line float-vector float-vector)
  (set-ftype make-plane plane (normal float-vector) (point float-vector) (distance number))
  (set-ftype make-cube body number number number (name object) (color object))
  (set-ftype make-prism body cons sweep-vec (name object) (color object))
  (set-ftype make-cylinder body number number (segments int) (name object) (color object))
  (set-ftype make-cone body float-vector bottom-plane (segments int) (name object) (coloring object))
  (set-ftype make-solid-of-revolution body cons (segments int) (name object) (color object))
  (set-ftype make-torus body cons (segments int) (name object) (color object))
  (set-ftype make-icosahedron body (optional number))
  (set-ftype make-dodecahedron body (optional number))
  (set-ftype make-gdome body body)
  ;; grahamhull などない
  ;; grahamhull
  (set-ftype quickhull cons cons) ; fvec の list -> fvec の list
  (set-ftype convex-hull-3d cons cons)
  (set-ftype make-body-from-vertices body cons)
  (set-ftype face+ face face face)  ; 交差領域ないと face 以外の何かが返るかも
  (set-ftype face* face face face)
  (set-ftype cut-body cons body plane)
  (set-ftype body+ body body body (repeat body))
  (set-ftype body- body body body)
  (set-ftype body* body body body)
  (set-ftype body/ body body plane)
  (set-ftype body-interference cons (repeat body))
  ;; constrained-motion
  ;; constrained-force
  ;; draw-constraint
  ;; draw-motion
  
  ;;; 視界とグラフィックス  ref:p.145
  ;; homo-viewport-clip などない
  ;; homo-viewport-clip
  (set-ftype view viewer  (size number) (width number) (height number) (x number) (y number)
                          (title object) (border-width number) (background number)
                          (viewpoint float-vector) (target float-vector) (viewdistance number)
                          (hither number) (yon number) (screen number) (screen-x number) (screen-y number)
                          (xcenter number) (ycenter number))
  ;; draw
  ;; draw-axis
  ;; draw-arrow
  ;; hid
  ;; hidd
  ;; hid2
  ;; render
  ;; make-light-source
  ;; tektro
  ;; kdraw
  ;; pictdraw
  ;; hls2rgb
  ;; rgb2hls
  ;; pixmap-animation
  ;; playback-pixmaps
  ;; hid-lines-animation
  ;; playback-hid-lines
  ;; list-visible-segments
  
  ;;; Xwindow ref:p.156
  ;; xflush
  ;; make-xwindow
  ;; init-xwindow
  ;; font-id
  ;; textdots
  ;; find-visual
  
  ;;; XToolKit  ref:p.170
  ;; next-event
  ;; event-type
  ;; event-window
  ;; event-x
  ;; event-y
  ;; event-width
  ;; event-height
  ;; event-state
  ;; display-events
  ;; window-main-loop
  ;; window-main-thread
  ;; make-text-window-stream
  ;; expand-tab

  ;;;;;;;;;;;;
  ;;; 拡張 ;;;
  ;;;;;;;;;;;;

  (cons fenv type-ineq-lt)) ; dot-pair を return
) ; for defun