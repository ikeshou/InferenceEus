#|
type_one.l で *receiver-mode* を指定した場合に必要となる
|#
;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")


(require :FIX "fixed_hash.l")
(require "type_class_tree.l")


(defparameter *method-to-class-or-group*
  (let ((ht (fix:make-hash-table :size 1024))
        (method-to-classes (fix:make-hash-table :size 1024)))
    ;; まずは method-to-classes を構築
    (dolist (cls (system:list-all-classes))
      ; :methods によりこのクラスの階層で定義されたメソッドたちを得る
      ; どうもオーバーロード定義したらここに登録されているっぽい。
      ; geometry:coordinates は propertied-object を継承しているが、どちらも :methods で :name がある...
      (dolist (method-pair (send cls :methods))     ; (:init compiled-code-object) の形
        (let* ((selector (car method-pair))
               (registered (fix:gethash selector method-to-classes)))
          (assert (keywordp selector) "not a method?")
          (assert (listp registered) "registered in a wrong way")   ; nil or list of class
          ; object である時、もしくは親以上のクラスでそのメソッドを持っていない時
          (when (or (equal cls object) 
                    (null (assoc selector (send (send cls :super) :all-methods))))
            (if (null registered)
              (fix:sethash selector method-to-classes (list cls))
              (fix:sethash selector method-to-classes (cons cls registered)))
          ))))
    ;; method-to-classes を変換
    (fix:maphash
      #'(lambda (selector cls-lst)
            (if (= (length cls-lst) 1)
              (fix:sethash selector ht (car cls-lst))
              (fix:sethash selector ht (make-instance group :class-list cls-lst))))
      method-to-classes)
    ht))


;;; heavily used in (infer-receiver-precise) in type_one.l
(defun method-to-class-or-group (method)
  "
  メソッド名からそのメソッドを持つ全クラスの集合を表現するクラスオブジェクトないし group のインスタンスを返す
  サブクラスなどが無意味に登録されていないことが保障される
  "
  (assert (keywordp method) "method-to-class-or-group(): arg should be a keyword")
  (assert (fix:gethash method *method-to-class-or-group*) "cannot find a class. wrong method name?")
  (fix:gethash method *method-to-class-or-group*))
