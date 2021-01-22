#|
クラスの集合を表現したり疑似クラスを表現したりするためのモジュール
型不等式を整理する際に必要となる、それらに対する操作関数もここで定義されている
|#

;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")


(require :fix "fixed_hash.l")
(require "type_util.l")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 型を表現するためのクラス定義 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Euslisp における唯一のオブジェクトでない数値たちを扱う疑似クラス
;;; object のサブクラスであると考えるとわかりやすい。object を引数にとる関数は数値も受けつけることがほとんど。
;;; (copy-object, identity, print など)
(defclass number :super object)
(defclass int :super number)
(defclass float :super number)
(defclass rational :super number)

;;; 型を細分化
(defclass mat :super array) ; 2 次元実数配列。matrix 関数が紛らわしいので mat で
(defclass bool :super symbol)
(defclass nil-class :super bool)  ; 変数代入時の型の類推の際に特別扱いし、型を決定しないようにする
(defclass t-class :super bool)

;;; 独自に定義したクラス ((subclassp x original) で一発で判定できる)
;;; 型不等式をたてる際にスキップするべきことを意味する any と推論の失敗を意味する never
(defclass original :super object)
(defclass any-class :super original)
(defclass never-class :super original)

;;; Union を表現するためのクラス group とそのインスタンス ((derivedp x group) で一発で判定できる)
;;; class-list には Union をとるクラスオブジェクトのリストが入る。
;;; *この要素は常に disjoint であるようにする*
;;; type_data.l でヘビーに参照される。リファクタリング時に注意
(defclass group :super object :slots (class-list))
(defconstant seq (make-instance group :class-list (list cons vector array)))
(defconstant dim (make-instance group :class-list (list int cons))) ; make-array
(defconstant axis (make-instance group :class-list (list float-vector int symbol nil-class))) ; rotate 系. fvec, 0, 1, 2, :x, :y, :z, nil が軸指定で許されることが多い
(defconstant sweep-vec (make-instance group :class-list (list float-vector number))) ; make-prism
(defconstant bottom-plane (make-instance group :class-list (list cons number))) ; make-cone
(defconstant veclike (make-instance group :class-list (list vector array)))
(defconstant pathlike (make-instance group :class-list (list pathname string)))
(defconstant packlike (make-instance group :class-list (list symbol string package)))
(defconstant symlike (make-instance group :class-list (list symbol string)))



;;;;;;;;;;;;;;;
;;; 操作関数 ;;;
;;;;;;;;;;;;;;;


;;; 定義済みのクラス (group のインスタンスは含まない。シンボルではなくクラスオブジェクト) を列挙した list
;;; bool など上記クラスの定義後に作成することが必要
(defparameter *class-list* (system:list-all-classes))
;;; 上記の vector
(defparameter *class-vec* (apply #'vector *class-list*))
;;; クラス数
(defparameter *class-num* (length *class-vec*))
;;; {クラスオブジェクト: インデックス} なる辞書
(defparameter *class-index-dict*
  (let ((ht (fix:make-hash-table :size (* *class-num* 2))))
    (dotimes (i *class-num*)
      (fix:sethash (svref *class-vec* i) ht i))
    ht))


(defun index-to-class (num) (svref *class-vec* num))


(defun class-to-index (cls)
  (assert (fix:gethash cls *class-index-dict*) "class-to-index(): no class found")
  (fix:gethash cls *class-index-dict*))


;;; 親クラス -> 子クラスへ有向辺を張ったときの、クラスツリーの隣接リスト表現 (vector of list)
(defparameter *class-tree*
  (let* ((size *class-num*)
         (adj (instantiate vector size)))
    ;; u, v はクラスのインデックス表記 (int)
    ;; u (parent) -> v (child) へ辺を張るよう adj へ追記
    ;; list に出現する数値の順番は不問なので先頭に追加         
    (flet ((add-edges (u v)
            (push v (svref adj u))))
        (dolist (child *class-list*)
          (unless (equal child object)  ; object の super はない。それ以外について
            (add-edges (class-to-index (send child :super))
                       (class-to-index child)))))
    adj))


;; reachable-list-by-index の補助関数
;; also used in sym-subst in type_one.l
(defun group-or-classp (x) (or (derivedp x group) (classp x)))


;; reachable-list-by-group-or-class の補助関数
(defun reachable-list-by-index (num)
  "
  クラスのインデックス表記 num を受け取り、そのクラスの全サブクラスのインデックスをリストにまとめて返す
  *class-tree* において num ノードから到達可能なノードのリストを返すとも言える
  "
  (let ((reachable nil)
        (visited (instantiate vector *class-num*))) ; 訪問済みなら t を記録。木なのでいらないけど
    (labels ((dfs (u)
              (assert (null (svref visited u)) "broken dfs")   ; 未訪問チェック
              (setf (svref visited u) t)
              (push u reachable)
              (dolist (v (svref *class-tree* u))
              (if (null (svref visited v))
                  (dfs v)
                  (format t "warn: *class-tree* is not tree?~%")))    ; 木なので常に未訪問なはず
            ))
        ; num を開始点として探索
        (dfs num))
    reachable))


;; class-intersec の補助関数
;; experiment.l でも使用される
(defun reachable-list-by-group-or-class (cls-exp)
  "
  クラスオブジェクトまたは group のインスタンスを受け取り、
  そのクラス集合の全サブクラスのインデックスをリストにまとめて返す
  "
  (assert (group-or-classp cls-exp) "group or class expected")
  (if (classp cls-exp)
    (reachable-list-by-index (class-to-index cls-exp))
    (let ((buf nil)
          (lst nil))
      (dolist (cls (group-class-list cls-exp))  ; slot access
        (setq lst (reachable-list-by-index (class-to-index cls)))
        ;; group の class-list は集合として disjoint であるようにしているので
        (assert (null (intersection lst buf :test #'=)) "class-list is not disjoint?")
        (push-cons-cons lst buf))
      buf)))


;; class-intersec の補助関数
(defun index-list-to-group-or-class (ind-lst)
  "
  クラスの集合を表すインデックスのリストを受け取り、
  クラスオブジェクトないし disjoint union な class-set を用いて表現された group インスタンスを返す
  ind-lst が nil の場合、never-class を適切に返す
  "
  (if (null ind-lst)
    never-class
    (let ((cls-ind-lst nil)
          (visited (instantiate vector *class-num*))) ; 訪問済みなら t を記録。木なのでいらないけど
        ;; 探索は Top である object を示すルートノードから始める
        ;; u から到達可能な全てのノードが ind-lst に含まれているなら cls-ind-lst に u を登録
        ;; 全てではないが一部が含まれているなら、そのサブタイプのいずれかが class-list に入るべきである。子ノードを再帰的に探索
        ;; 全く含まれていないなら、そのノードでの探索を終了
        (labels ((dfs-coloring (u)
                  (assert (null (svref visited u)) "broken dfs-coloring")   ; 未訪問チェック
                  (setf (svref visited u) t)
                  (let ((reachable (reachable-list-by-index u)))
                    (cond
                      ((subsetp reachable ind-lst)
                        (push u cls-ind-lst))
                      ((not (null (intersection reachable ind-lst)))
                        (dolist (v (svref *class-tree* u))
                          (if (null (svref visited v))
                            (dfs-coloring v)
                            (format t "warn: *class-tree* is not tree?~%")))    ; 木なので常に未訪問なはず    
                        )
                      (t nil))))    ; 何もしない
                )
            ; ルートを開始点として探索
            (dfs-coloring (class-to-index object)))
        ;; インデックス表記のリストの要素が一つのみの場合、クラスオブジェクトを返す
        ;; それ以外の場合、インデックス表記のリストからクラスオブジェクトのリストに変換して group 生成
        (if (= (length cls-ind-lst) 1)
          (index-to-class (car cls-ind-lst))
          (make-instance group :class-list (mapcar #'index-to-class cls-ind-lst)))
    )))



;; used in unify-ineq in type_one.l
(defun class-intersec (a b)
  "
  a, b はクラスオブジェクトないし group のインスタンス
  順序関係であるサブクラス関係において、X <: a and X <: b なる最大のクラスないしグループ X を求める
  言い換えると、a, b の積集合を表すクラスオブジェクトないし group のインスタンスを返す
  空集合となった場合 never-class を適切に返す
  "
  ;; a, b に any-class, never-class が入るときは先に処理する
  ;; (これらは特別な意味をもつ。都合上 object を継承させているが他のクラス同等の文脈で扱ってはいけない)
  ;; any-class はわからんけど、制約に never-class が入るときはある
  ;; (class-intersec が失敗すると never が帰るので、呼び出しもとの unify-ineq にて制約に追加される)
  (assert (and (group-or-classp a) (group-or-classp b)) "class-intersec(): arg should be a class or group")
  (cond
    ((or (equal a never-class) (equal b never-class)) never-class)
    ((equal a any-class) b)
    ((equal b any-class) a)
    (t (index-list-to-group-or-class
         (intersection (reachable-list-by-group-or-class a) (reachable-list-by-group-or-class b))))
  ))



;; used in calc-return-union in type_one.l
(defun class-union (a b)
  "
  a, b はクラスオブジェクトないし group のインスタンス
  順序関係であるサブクラス関係において、 a <: X and b <: X なる最大のクラスないしグループ X を求める
  言い換えると、a, b の和集合を表すクラスオブジェクトないし group のインスタンスを返す
  "
  (assert (and (group-or-classp a) (group-or-classp b)) "class-union(): arg should be a class or group")
  (cond
    ((or (equal a never-class) (equal b never-class)) never-class)  ; bottom 的な扱いだと never を返さないべき？
    ((equal a any-class) b)
    ((equal b any-class) a)
    (t (index-list-to-group-or-class
         (union (reachable-list-by-group-or-class a) (reachable-list-by-group-or-class b))))
  ))



;; class-lca の補助関数
(defun extract-class-list (cls-exp)
  "クラスオブジェクトないし group のインスタンスを受け取り、クラスオブジェクトのリストを生成して返す"
  (assert (group-or-classp cls-exp) "extract-class-list(): arg should be a class or group")
  (if (classp cls-exp)
    (list cls-exp)  ; wrap
    (group-class-list cls-exp)))    ; slot access


;; common-super の補助関数
(defun depth (x)
  "クラスツリーにおいてクラス x のノード深さを求める。(object は 0)"
  (assert (classp x) "depth(): arg should be a class")
  (let ((i 0))
    (while (not (equal x object))
      (setq x (send x :super))
      (incf i))
    i))


;; common-super の補助関数
(defun n-super (x num)
  "x の num 個祖先のクラスを求める"
  (assert (classp x) "n-super(): arg should be a class")
  (assert (<= num (depth x)) "n-super(): too large n")
  (dotimes (i num)
    (setq x (send x :super)))
  x)

;; common-super の補助関数
(defun all-samep (lst)
  "リストの要素が全て等しいか判定する"
  (= (length (remove-duplicates lst :test #'equal)) 1))


;; class-lca の補助関数
(defun common-super (cls-lst)
  "クラスオブジェクトのリストを受け取り、それらの共通の親クラスのうち最小のものを返す"
  (let* ((depth-lst 
           (mapcar #'depth cls-lst))
         (min-depth
           (apply #'min depth-lst))
         (same-depth-cls-lst
           (mapcar #'(lambda (d cls) (n-super cls (- d min-depth)))
                   depth-lst
                   cls-lst)))
    (while (not (all-samep same-depth-cls-lst))
      (setq same-depth-cls-lst (mapcar #'(lambda (x) (send x :super)) same-depth-cls-lst)))
    ;; 全てが共通の親クラスに揃った
    (car same-depth-cls-lst)))



;; used in unify-ineq in type_one.l
(defun class-lca (a b)
  "
  a, b はクラスオブジェクトないし group のインスタンス
  順序関係であるサブクラス関係において、X :> a and X :> b なる最小のクラス X を求める
  a, b の class-list で出現する全てのクラスに対して親クラスとなるようなクラスを返す
  (class-union と異なり group のインスタンスが返ることはない。
  Union(S, T) <: var は S と T の上方の交わり U として U <: var と同義なのでそこまで整理する)
  "
  (assert (and (group-or-classp a) (group-or-classp b)) "class-lca(): arg should be a class or group")
  (cond
    ((or (equal a never-class) (equal b never-class)) never-class)  ; bottom 的な扱いだと never を返さないべき？
    ((equal a any-class) b)
    ((equal b any-class) a)
    (t (common-super
          (union (extract-class-list a) (extract-class-list b))))
  ))

