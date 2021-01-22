#|
type_one.l で *condition-mode* を指定した場合に必要となる
|#
;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")


(defparameter *dynamic-dispatch-funcs*
  '(derivedp subclassp class classp numberp integerp floatp atom
    ;; null
    functionp compiled-function-p symbolp keywordp packagep
    listp consp vectorp arrayp integer-vector-p float-vector-p stringp bit-vector-p
    hash-table-p streamp input-stream-p output-stream-p io-stream-p
    readtable-p pathnamep 
    matrixp coordinates-p bodyp facep
    ))


;;; heavily used in (infer-flow-precise) in type_one.l
(defun dynamic-dispatch-p (sexp)
  "
  sexp を flatten してその中にクラス判別関数の典型的なものが存在するか調べる
  本当なら同一名の大域局所変数や、同一名の局所関数は弾かなくてはならないが、名前が衝突しないと仮定している
  "
  (not (null (intersection *dynamic-dispatch-funcs* (flatten sexp)))))

