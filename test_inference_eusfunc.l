(require "inference_eusfunc.l")
(in-package :INFERENCE-EUSFUNC)

#| ここを臨機応変にいじろう! |#
;;; 0 → errorのみをキャッチする, 1 → passしたケースも全て出力していく。
(defvar *debug-mode* 1)


;;; testが通っていないケースをカウント
(defvar *error-count* 0)
;;; debug-modeが1の時はここにカウント結果が溜まっていき最後に出力される。
(defvar *passed-count* 0)

;;; assertion errorとなったテスト番号が追加されていく
(defparameter *error-test-num* nil)

;;; vがtであるか判定。nilならErrorを出力する。
(defun assert-true (v &optional msg)
    (if (null v)
        (progn
            (format t "Error ~a occurred when t-checking ~a" (if (null msg) "" msg) v)
            (terpri)
            (incf *error-count*)
            (push (+ *error-count* *passed-count*) *error-test-num*))
        (if (= *debug-mode* 1) 
            (progn
              (format t "OK! (t expected. got ~a)" v)
              (terpri)
              (incf *passed-count*)))
    ))

;;; v1とv2が構造的に等しいか判定。nilならErrorを出力する。
(defun assert-equal (v1 v2 &optional msg)
  (if (equal v1 v2)
      (if (= *debug-mode* 1)
          (progn
            (format t "OK! (~a expected. got ~a)" v2 v1)
            (terpri)
            (incf *passed-count*)))
      (progn (format t "Error: ~a" (if (null msg) "" msg))
              (terpri)
              (format t "got---~%")
              (print v1)
              (print v2)
              (format t "expected---~%")
              (incf *error-count*)
              (push (+ *error-count* *passed-count*) *error-test-num*))))



;;; --------------------------------------
;;; 書ける範囲でユニットテストを書いていく
;;; --------------------------------------

;;; unit test for union-classp
(assert-equal (union-classp object) nil)    ; 普通のクラスは union-class ではない
(assert-equal (union-classp nil-class) nil)    ; nil-class は union-class ではない
(assert-equal (union-classp number-class) nil)    ; 数値関連も union-class ではない
(assert-true (union-classp sequence-class))
(assert-true (union-classp pathlike-class))
(assert-true (union-classp symbollike-class))
(assert-true (union-classp packagelike-class))


;;; unit test for union-to-component
(assert-equal (union-to-component sequence-class) (list cons vector))    ; 要素はクラスオブジェクト (シンボルではない)


;;; unit test for push-cons-al
(let ((tmp0 nil)
      (tmp1 '((a . 1)))
      (tmp2 '((a . 1) (b . 2))))
  ; nil の push では変化しない
  (push-cons-al nil tmp0)
  (assert-equal tmp0 nil)
  (push-cons-al nil tmp1)
  (assert-equal tmp1 '((a . 1)))
  (push-cons-al nil tmp2)
  (assert-equal tmp2 '((a . 1) (b . 2)))
  ; cons の push で alist を保ち追加されるか
  (push-cons-al '(c . 3) tmp0)
  (assert-equal tmp0 '((c . 3)))
  (push-cons-al '(c . 3) tmp1)
  (assert-equal tmp1 '((c . 3) (a . 1)))
  (push-cons-al '(c . 3) tmp2)
  (assert-equal tmp2 '((c . 3) (a . 1) (b . 2)))
)


;;; unit test for push-al-al
(let ((tmp0 nil)
      (tmp1 '((a . 1)))
      (tmp2 '((a . 1) (b . 2))))
  ; nil の push では変化しない ( nil の alist とは (nil) ではなく nil と捉えている。)
  (push-al-al nil tmp0)
  (assert-equal tmp0 nil)
  (push-al-al nil tmp1)
  (assert-equal tmp1 '((a . 1)))
  (push-al-al nil tmp2)
  (assert-equal tmp2 '((a . 1) (b . 2)))
  ; alist の push で alist を保ち追加されるか
  (push-al-al '((c . 3)) tmp0)
  (assert-equal tmp0 '((c . 3)))
  (push-al-al '((c . 3)) tmp1)
  (assert-equal tmp1 '((c . 3) (a . 1)))
  (push-al-al '((c . 3)) tmp2)
  (assert-equal tmp2 '((c . 3) (a . 1) (b . 2)))
)


;;; unit test for builtin-p
(assert-true (builtin-p 'append))
(assert-equal (builtin-p 'non-existent-function) nil)


;;; unit test for get-type
(assert-equal (get-type 'x `((y . ,vector) (x . ,object))) object)
(assert-equal (get-type 'z `((y . ,vector) (x . ,object))) nil)    ; 環境になかったら nil


;;; unit test for typing
;;;                          env             ftypes     sym flag(optional)
(assert-equal (typing *global-type-env* *global-ftypes* nil nil) nil-class)
(assert-equal (typing *global-type-env* *global-ftypes* t) symbol)
(assert-equal (typing *global-type-env* *global-ftypes* 'eq t) (list symbol (list object object)))
(assert-equal (typing *global-type-env* *global-ftypes* '*eusdir*) string)
(assert-equal (typing *global-type-env* *global-ftypes* 5) integer-class)
(assert-equal (typing *global-type-env* *global-ftypes* 5.0) float-class)
(assert-equal (typing *global-type-env* *global-ftypes* 1/2) rational-class)
(assert-equal (typing *global-type-env* *global-ftypes* #i(1 2 3)) integer-vector)


;;; unit test for variablep
(assert-true (variablep (gensym)))
(assert-equal (variablep t) nil)
(assert-equal (variablep nil) nil)



;;; extract 系以外でユニットテストできるやつ
;;; unit test for find-opponent
(assert-equal (find-opponent 'x '((x . hoge) (z . hoge) (hoge . moo) (x . y) (y . piyo) (y . x))) 'y)
(assert-equal (find-opponent 'x '((x . hoge) (z . hoge) (hoge . moo) (x . y) (hoge . x) (y . x))) 'hoge)    ; 複数見つかったときは最初のやつ

;;; unit test for my-superclassp
; builtin
(assert-true (my-superclassp vectorclass vectorclass))
(assert-true (my-superclassp object metaclass))
(assert-equal (my-superclassp vectorclass symbol) nil)
(assert-equal (my-superclassp metaclass object) nil)
; Union
(assert-true (my-superclassp sequence-class sequence-class))    ; Union で同一クラス
(assert-true (my-superclassp pathlike-class pathlike-class))
(assert-true (my-superclassp symbollike-class symbollike-class))
(assert-true (my-superclassp packagelike-class packagelike-class))
(assert-true (my-superclassp object sequence-class))    ; Union が負ける
(assert-true (my-superclassp object pathlike-class))
(assert-true (my-superclassp object symbollike-class))
(assert-true (my-superclassp object packagelike-class))
(assert-true (my-superclassp sequence-class cons))    ; Union が勝つ
(assert-true (my-superclassp sequence-class vector))
(assert-true (my-superclassp sequence-class bit-vector))
(assert-true (my-superclassp sequence-class string))
(assert-equal (my-superclassp sequence-class array) nil)    ; sequence-class と array は無関係
(assert-true (my-superclassp pathlike-class string))
(assert-true (my-superclassp pathlike-class pathname))
(assert-true (my-superclassp symbollike-class string))
(assert-true (my-superclassp symbollike-class symbol))
(assert-true (my-superclassp packagelike-class string))
(assert-true (my-superclassp packagelike-class package))
(assert-equal (my-superclassp symbollike-class packagelike-class) nil)    ; intersection はあるがサブタイプではないので nil
; 数値追加クラス
(assert-true (my-superclassp number-class number-class))
(assert-true (my-superclassp number-class float-class))
(assert-equal (my-superclassp symbol number-class) nil)
; nil-class
(assert-true (my-superclassp nil-class nil-class))        ; nil-class は nil-class に対してしか superclass になりえない。(consとsymbolのintersectionの型のサブタイプなんて他に存在しない)
(assert-equal (my-superclassp nil-class cons) nil)
(assert-equal (my-superclassp nil-class object) nil)
(assert-true (my-superclassp cons nil-class))    ; (nil-class はcons, symbolに負けてほしい)
(assert-true (my-superclassp symbol nil-class))
(assert-equal (my-superclassp pathname nil-class) nil)    ; 何にでも負けていいわけではない


;;; unit test for specify-class
(assert-equal (specify-class object cons) cons)
(assert-equal (specify-class cons object) cons)
(assert-equal (specify-class object object) object)
(assert-equal (specify-class sequence-class sequence-class) sequence-class)
(assert-equal (specify-class sequence-class cons) cons)
(assert-equal (specify-class cons sequence-class) cons)
(assert-equal (specify-class sequence-class float-vector) float-vector)
(assert-equal (specify-class float-vector sequence-class) float-vector)
(assert-equal (specify-class sequence-class symbollike-class) string)
(assert-equal (specify-class symbollike-class sequence-class) string)
(assert-equal (specify-class nil-class nil-class) nil-class)
(assert-equal (specify-class cons nil-class) nil-class)
(assert-equal (specify-class symbol nil-class) nil-class)


;;; unit test for convert-to-ordinary-type
(let ((type-var-to-type (list (cons 'a integer-class) (cons 'b vector) (cons 'c nil-class) (cons 'd object))))
  (assert-equal (convert-to-ordinary-type integer-class type-var-to-type) "INTEGER-CLASS")    ; atom, 既にクラスオブジェクト
  (assert-equal (convert-to-ordinary-type 'b type-var-to-type) "VECTOR")    ; atom, 型変数
  (assert-equal (convert-to-ordinary-type (list 'c metaclass) type-var-to-type) '("NIL-CLASS" "METACLASS"))    ; cons, 既にクラスオブジェクト + 型変数
  (assert-equal (convert-to-ordinary-type '(c d) type-var-to-type) '("NIL-CLASS" "OBJECT"))    ; cons, 型変数
  ; optional, rest, key 全部のせ
  (assert-equal (convert-to-ordinary-type '(a (optional . b) (rest) (key1 . c) (key2 . d)) type-var-to-type) '("INTEGER-CLASS" (optional . "VECTOR") (rest) (key1 . "NIL-CLASS") (key2 . "OBJECT")))
)


;;; extract 系でユニットテストできるやつ



;;; ----------------------------------
;;; メインの型推論部分のミニ統合テスト
;;; ----------------------------------

;;; コード片を実行して内部変数をチェックしていくのもテストで有効
(setq type-equations nil)


;;; とりま型がつくか単純な関数 (ボディ部では関数コールのみ、マクロや特殊形式なし) で実験する 

(setf (BUGFIXED-HASH::gethash 'dot-cons-p *user-functions*) t) 
(push-cons-al (cons 'dot-cons-p (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun dot-cons-p (dot-cons)
  (listp (cdr dot-cons)))
)) type-equations)

(setf (BUGFIXED-HASH::gethash 'access-symbol *user-functions*) t)
(push-cons-al (cons 'access-symbol (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun access-symbol (sym)
  (eval sym))
)) type-equations)

(setf (BUGFIXED-HASH::gethash 'vector-arg-ret-0 *user-functions*) t) 
(push-cons-al (cons 'vector-arg-ret-0 (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun vector-arg-ret-0 (vec)
  (svref vec 0))
)) type-equations)

(setf (BUGFIXED-HASH::gethash 'bit-vec-arg-ret-0 *user-functions*) t) 
(push-cons-al (cons 'bit-vec-arg-ret-0 (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun bit-vec-arg-ret-0 (bv)
  (bit bv 0))
)) type-equations)

(setf (BUGFIXED-HASH::gethash 'pathname-arg-ret-file-str *user-functions*) t) 
(push-cons-al (cons 'pathname-arg-ret-file-str (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun pathname-arg-ret-file-str (path)
      (pathname-name path))
)) type-equations)


;;; 無引数のときのチェック

(setf (BUGFIXED-HASH::gethash 'no-arg-ret-nil *user-functions*) t) 
(push-cons-al (cons 'no-arg-ret-nil (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun no-arg-ret-nil ()
  nil)
)) type-equations)

(setf (BUGFIXED-HASH::gethash 'no-arg-ret-int *user-functions*) t) 
(push-cons-al (cons 'no-arg-ret-int (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun no-arg-ret-int()
  1)
)) type-equations)


;;; repeat のチェック

(setf (BUGFIXED-HASH::gethash '2d-array-arg-ret-0-0 *user-functions*) t) 
(push-cons-al (cons '2d-array-arg-ret-0-0 (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun 2d-array-arg-ret-0-0 (arr)
   (aref arr 0 0))
)) type-equations)


;;; optional のチェック

(setf (BUGFIXED-HASH::gethash 'add-four *user-functions*) t) 
(push-cons-al (cons 'add-four (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun add-four (a b &optional c (d 4))
    (+ a b c d))
)) type-equations)


;;; rest のチェック

(setf (BUGFIXED-HASH::gethash 'sumup-head2 *user-functions*) t) 
(push-cons-al (cons 'sumup-head2 (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun sumup-head2 (a &rest ls)
  (+ a (car ls)))
)) type-equations)

;;; key のチェック

(setf (BUGFIXED-HASH::gethash 'key-lover *user-functions*) t) 
(push-cons-al (cons 'key-lover (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun key-lover (a &key b (c (list 1 2 3)))
   (+ (length a) (length b) (length c))
)
)) type-equations)


;;; ユーザー関数を内部で用いるもの (逐次実行の際は innner-1 と outer-1 を両方登録した上でテストをすること)
(setf (BUGFIXED-HASH::gethash 'inner-1 *user-functions*) t) 
(push-cons-al (cons 'inner-1 (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun inner-1 () 
  (cons 1 2))
)) type-equations)

(setf (BUGFIXED-HASH::gethash 'outer-1 *user-functions*) t) 
(push-cons-al (cons 'outer-1 (list (gensym) (gensym))) *global-ftypes*)
(push-al-al
(car (extract-eq *global-type-env* *global-ftypes*
'(defun outer-1 ()
  (inner-1))
)) type-equations)


;;; ボディ部で let, let* を用いるもの



(setq dot-lst (preprocessing type-equations *global-ftypes*))
(setq separated-equations (car dot-lst))
(setq user-func-alist (cdr dot-lst))
(setq type-var-to-type (solve-type-equations separated-equations user-func-alist))

(setq hsh 
  (let ((user-func-dictionary (BUGFIXED-HASH::make-hash-table)))
    (dolist (fn-type user-func-alist)
      (let* ((fn (car fn-type))
             (result-type (car (cdr fn-type)))
             (arg-type (car (cdr (cdr fn-type))))
             (converted-result-type (convert-to-ordinary-type result-type type-var-to-type))
             (converted-arg-type (convert-to-ordinary-type arg-type type-var-to-type)))
        (setf (BUGFIXED-HASH::gethash fn user-func-dictionary) (list converted-result-type converted-arg-type))))
    user-func-dictionary))



;;; assertion test
(assert-equal (gethash 'dot-cons-p hsh) '("SYMBOL" ("CONS")))
(assert-equal (gethash 'access-symbol hsh) '("OBJECT" ("OBJECT")))
(assert-equal (gethash 'vector-arg-ret-0 hsh) '("OBJECT" ("VECTOR")))
(assert-equal (gethash 'bit-vec-arg-ret-0 hsh) '("INTEGER-CLASS" ("BIT-VECTOR")))
(assert-equal (gethash 'pathname-arg-ret-file-str hsh) '("STRING" ("PATHLIKE-CLASS")))
(assert-equal (gethash 'no-arg-ret-nil hsh) '("NIL-CLASS" ("NIL-CLASS")))    ; 無引数はどうか?
(assert-equal (gethash 'no-arg-ret-int hsh) '("INTEGER-CLASS" ("NIL-CLASS")))
(assert-equal (gethash '2d-array-arg-ret-0-0 hsh) '("OBJECT" ("ARRAY")))    ; repeat はどうか?
(assert-equal (gethash 'add-four hsh) '("NUMBER-CLASS" ("NUMBER-CLASS" "NUMBER-CLASS" (optional . "NUMBER-CLASS") (optional . "INTEGER-CLASS"))))    ; optional はどうか? (repeatもチェックできているけど)
(assert-equal (gethash 'sumup-head2 hsh) '("NUMBER-CLASS" ("NUMBER-CLASS" (rest))))    ; rest はどうか?
(assert-equal (gethash 'key-lover hsh) '("NUMBER-CLASS" ("SEQUENCE-CLASS" (b . "SEQUENCE-CLASS") (c . "CONS"))))    ; key はどうか?
(assert-equal (gethash 'outer-1 hsh) '("CONS" ("NIL-CLASS")))    ; 内部でユーザー関数を使うケース


;;; -------------------------
;;; -------------------------
(terpri)
(format t "========================================================~%")
(if (> *error-count* 0)
    (progn 
      (format t "Failure: ~a errors in ~a~%" *error-count* *error-test-num*)
      (if (= *debug-mode* 1)
        (format t "passed: ~a test cases~%" *passed-count*)))
    (format t "* Euslisp inference UnitTest / mini-IntegrationTest OK *~%"))
(format t "========================================================~%")


;;; memo
;;; (optional α) と (optional . α) の設定がごっちゃになっていたので全てを後者で書き直した (つもり) 。もしかするとまだバグがあるかも。