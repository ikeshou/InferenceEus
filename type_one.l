#|
関数の引数型、戻り値型の上界を求めるアルゴリズム
geometry:body などの user 領域に展開されるクラスたちに対応するためパッケージはあえて切らない
(type_data.l で全ての幾何学クラスに prefix をつけなくてはいけなくなり面倒)

> irteusgl
> load "type_one.l"
> (infer-file "file_name_here")
|#
;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")0


(require :FIX "fixed_hash.l")
(require "env.l")
(require "type_util.l")
(require "type_class_tree.l")
(require "infer_receiver_helper.l")
(require "infer_flow_helper.l")
(require "type_data.l")



;;; level of inference system
;;; 最低限のモード (nil) では (send ...) や (if (test) ...) の ... 部分は飛ばされてしまう
;;; 
;;; *receiver-mode* => 
;;; (send x :method arg) の x を推論する
;;; *flow-mode* => 
;;; (if (test) (s1) (s2)) などの条件分岐で「テスト部分にクラス判定関数がなかったら」分岐全てを見て制約を立式
;;; 戻り値は各式の戻り値の Union とする
(defconstant *receiver-mode* 
  ;; nil  
  t     ; for better inference
)
(defconstant *flow-mode*
  ;; nil
  t     ; for better inference
)


;;; <: 型不等式. ?a <: T のとき ((a T) ...) という alist となる. 常に要素の cadr には class or group が入る
;;; assoc するときは nil チェックをすること。 上界が指定されていないフリーの型変数は登録されていない。(?a <: object だが)
(defparameter *type-ineq-lt* nil)
;;; :> 型不等式. ?a :> T のとき ((a T) ...) という alist となる. 常に要素の cadr には class or group が入る
;;;（使わないはず）
(defparameter *type-ineq-gt* nil)



;;; user interface
(defun infer-file (fname)
  "
  Args:
      fname (str)
  Returns:
      fix:hash-table
          各ユーザー関数シンボルをキーに、関数型をバリューにもった辞書
          導入された型変数は上界の型で置換されている。型はクラスオブジェクトまたは group のインスタンス
  Variables:
      sexp-vec: vector. ソースファイルの S 式の単位を配置したもの
      user-vars, user-funcs: fix:hash-table. key は symbol, val は int (ソースの S 式の並びの中での出現位置)
      arranged-vec: vector. ワンパスで推論できるよう関数定義と変数定義を並び替え、 S 式を再配置したもの
      sexps: list. arranged-vec をリストに変換し progn で包んだもの。推論のターゲット
      global-varenv, global-fenv: env (fix:hashtable ではない). 大域変数、大域関数の型環境
  "
  ;;; load は諸説あり？
  ;;; 定義済みの大域変数、大域関数を集める際に :USER を除いているので問題は起きないはず
  ;;; pros) 実行に時間がかかるかも。
  ;;; cons) クラスツリーの作成時にシステム関数 system:list-all-classes などが使えて便利。
  (load fname)
  ;;; 変数環境も変わるしクラスも増えるので強制的にもう一度読み込まないと
  (load "type_class_tree.l")
  (load "infer_receiver_helper.l")
  (load "type_data.l")
  (let* ((sexp-vec (source-to-sexp-vec fname))
         (user-vars (collect-user-vars sexp-vec))
         (user-funcs (collect-user-funcs sexp-vec))
         (arranged-vec (convert-to-dag sexp-vec user-vars user-funcs))
         (sexp `(progn ,@(coerce arranged-vec cons)))
         (global-varenv (build-varenv)) ; env
         (dot-pair (build-fenv))
         (global-fenv (car dot-pair)))  ; env
    ;; 型不等式を反映
    (setq *type-ineq-lt* (cdr dot-pair))
    ;; ソースの S 式を推論。global-fenv, *type-ineq-lt*, *type-ineq-gt* が破壊的に変更される
    (infer sexp global-varenv global-fenv)
    ;; ユーザー関数の型を引き、各引数と戻り値について具象型ならそれを、型変数なら上界を記録した辞書を作成
    (upper-bound-typing user-funcs (env-ht global-fenv))))



(defun upper-bound-typing (user-funcs ftype-dict)
  "f: ?a -> ?b, ?a <: S, ?b <: T のとき、f: S -> T で型を登録した辞書を返す"
  (let ((ht (copy-object user-funcs)))
    (maphash
      #'(lambda (fn v)  ; fn: user func symbol, v: t
          (let* ((type-pair (fix:gethash fn ftype-dict))
                 (arg-lst (car type-pair))
                 (ret (cdr type-pair))
                 (arg-subst nil)  ; arg-lst を上界変換したもの
                 (ret-subst (sym-subst ret))) ; ret を上界変換したもの
            (dolist (elm arg-lst) ; arg-lst は (S a0 (repeat . a1) (rest) ...) のような形
              (if (atom elm)
                (push (sym-subst elm) arg-subst)
                (progn
                  (assert (= (length elm) 1) "type notation broken (long cons)")
                  (assert (symbolp (car elm)) "type notation broken (not a symbol)")
                  (push
                    (if (equal elm '(rest))
                      elm
                      (cons (car elm) (sym-subst (cdr elm))))
                    arg-subst)
                  )))
            (setq arg-subst (reverse arg-subst))
            (fix:sethash fn ht (cons arg-subst ret-subst))))
      user-funcs)
    ht))


;;; upper-bound-typing の補助関数
(defun sym-subst (target)
  "
  引数が型変数のシンボルなら *type-ineq-lt* を検索し、上界で変換して返す
  target は int や ?a0 などの型を表す単項で、(optional . T) などの形ではこないとする
  "
  (assert (atom target) "sym-subst(): arg should be an atom")
  (cond
    ; 既にクラスに類する表現になっている
    ((group-or-classp target)
      (if (equal target any-class)
        object  ; 上界なし
        target))
    ; 型変数
    ((typevarp target)
      (if (assoc target *type-ineq-lt*)
        (cadr (assoc target *type-ineq-lt*))
        object))  ; 上界なし
    (t
      (error "unknown type symbol?"))))



;;; main
(defun infer (sexp varenv fenv)
  "
  対象の S 式の型を返す（型変数 ?a, ^a or クラスオブジェクト or group インスタンス）
  見る過程でたった型変数に対する型不等式は *type-ineq-lt*, *type-ineq-gt* に記録される
  見る過程で関数、変数定義がなされた場合は varenv, fenv に適宜登録される
  "
  ;; (format t "infer -> ~A~%" sexp)
  (assert (and (env-p varenv) (env-p fenv)) "infer(): not an env")
  (if (atom sexp)
    (infer-unary sexp varenv fenv)
    (let ((head (car sexp)))
      (cond
        ; EusLisp Core
        ((getenv head fenv)
          (infer-call sexp varenv fenv))
        ((member head '(prog1 progn))
          (infer-prog sexp varenv fenv))
        ((eq head 'quote)
          (infer-quote sexp varenv fenv))
        ((member head '(setq setf defvar defparameter defconstant))
          (infer-set sexp varenv fenv))
        ((member head '(let let*))
          (infer-let sexp varenv fenv))
        ((eq head 'defun)
          (infer-defun sexp varenv fenv))
        ((member head '(flet labels))
          (infer-flet sexp varenv fenv))
        ((eq head 'function)
          (infer-function sexp varenv fenv))
        ; for and while loop
        ((member head '(while until))
          (infer-while sexp varenv fenv))
        ((eq head 'loop)
          (infer-loop sexp varenv fenv))
        ((member head '(dotimes dolist))
          (infer-do sexp varenv fenv))
        ; block and exit
        ((member head '(block catch))
          (infer-block sexp varenv fenv))
        ((member head '(return-from throw))
          (infer-return-from sexp varenv fenv))
        ((eq head 'return)
          (infer-return sexp varenv fenv))
        ; other common macros
        ((member head '(push pushnew))
          (infer-push sexp varenv fenv))
        ((eq head 'pop)
          (infer-pop sexp varenv fenv))
        ((member head '(incf decf))
          (infer-incf sexp varenv fenv))
        ((eq head 'with-output-to-string)
          (infer-output-string sexp varenv fenv))
        ; 個々に対応する必要のある関数たち
        ; with or without using #'
        ((member head '(funcall apply))
          (infer-funcall sexp varenv fenv))
        ; instances
        ((member head '(instance make-instance))
          (infer-make-instance sexp varenv fenv))
        ((eq head 'instantiate)
          (infer-instantiate sexp varenv fenv))
        ; type specification
        ((eq head 'coerce)
          (infer-coerce sexp varenv fenv))
        ((eq head 'concatenate)
          (infer-concatenate sexp varenv fenv))
        ((eq head 'map)
          (infer-map sexp varenv fenv))
        ; conditional branch
        ((member head '(and or if when unless cond case))
          (infer-flow sexp varenv fenv))
        ; receiver
        ((member head '(send send*))
          (infer-receiver sexp varenv fenv))
        ; どうせ情報がないので飛ばすやつら
        ;; ((member head '(defclass defmethod))
        ;;   (format t "skipped class (defclass or defmethod): ~a~%" (cadr sexp)))
        ; よくわからず飛ばすやつ。Bug Fix に有用かもしれないため S 式を全部出力
        (t
          ;; (format t "skipped s-exp (unknown): ~a~%" sexp)
          any-class)))))



;;; 単項
(defun infer-unary (x varenv fenv &key (is-func nil))
  (assert (atom x) "infer-unary(): sexp should be an atom")
  (cond 
    ; シンボルのうち変数でないもの
    ((null x) nil-class)
    ((eq x 't) t-class)
    ; 関数 (#'cons などは '(funciton cons) と read されるので考えなくて良い)
    ((and is-func (symbolp x))
      (if (getenv x fenv) (getenv x fenv) any-class))  ; エントリになかった場合は any-class
    ; 変数
    ((symbolp x)
      (if (getenv x varenv) (getenv x varenv) any-class))  ; エントリになかった場合は any-class
    ; リテラルのうちオブジェクトでないもの
    ((integerp x) int)
    ((floatp x) float)
    ((numberp x) rational)
    ; リテラルの残り
    (t (class x))))  ; (send (class x) :name) では文字列表記が返ってしまいクラスオブジェクトは返らない



;;; formulate-actual の補助関数
(defun unify-ineq (ineq-pair ineq-alist &key (lt t))
  "
  1 本の型不等式 ineq-pair と型不等式の集合 ineq-alist を受け取り、型不等式をまとめて返す（非破壊）
  型不等式の要素は常に car に型変数が、cadr に具象型が入っており、戻り値もその条件を満足する
  型不等式の要素に同一型変数のエントリが複数登録されていることはない
  ineq-pair: 新しく立った型不等式. (?a0 S)
  ineq-alist: これまでに立った型不等式たちの alist. ((?a0 T) (?a1 U) ...)
  lt: type-ineq が <: かどうか. t のとき ((a T) ...) は a <: T を、nil のとき a :> T を意味する
  "
  (assert (and (= (length ineq-pair) 2) (symbolp (car ineq-pair))) "unify-ineq(): invalid ineq-pair")
  (let ((typevar (car ineq-pair))
        (from-cls (cadr ineq-pair))
        (unified-ineq (copy-tree ineq-alist)))  ; setf するのでコピーする
    (if (assoc typevar ineq-alist)
      ; 既に存在する型変数に対する制約
      (let ((to-cls (cadr (assoc typevar ineq-alist))))
        (if lt
          ; a <: from-cls and a <: to-cls => a <: Intersection(from-cls, to-cls)
          (setf (cadr (assoc typevar unified-ineq)) (class-intersec from-cls to-cls))
          ; a :> from-cls and a :> to-cls => a :> LeastCommonAncestor(from-cls, to-cls)
          (setf (cadr (assoc typevar unified-ineq)) (class-lca from-cls to-cls))))
      ; 新しい型変数に対する制約
      (push-cons-al ineq-pair unified-ineq))
    unified-ineq))


;;; infer-call の補助関数
(defun formulate-actual (formal-type actual-type)
  "
  formal-type :> actual-type（型変数ないし具象型）という式がたったとき、
  actual-type に関する不等式を *type-ineq-lt* に追記する
  (formal-type の上界で actual-type を上から抑えられる)
  "
  ;; (format t "formal ~A :> actual ~A~%" formal-type actual-type)
  (assert (or (typevarp formal-type) (group-or-classp formal-type)) "formulate-actual(): invalid formal type")
  (assert (or (typevarp actual-type) (group-or-classp actual-type)) "formulate-actual(): invalid actual type")
  (when (and (typevarp actual-type) 
             (or (group-or-classp formal-type) (assoc formal-type *type-ineq-lt*)))  ; 相手に制約がないならスルー
    (let* ((upper
              (if (group-or-classp formal-type)
                formal-type ; 型がびしっと決まっている
                (cadr (assoc formal-type *type-ineq-lt*))))
           (ineq-pair
            (list actual-type upper)))
      (setq *type-ineq-lt* (unify-ineq ineq-pair *type-ineq-lt* :lt t)))))


(defun formulate-formal (formal-type actual-type)
  "
  (今のところ使用しない)
  formal-type :> actual-type（型変数ないし具象型）という式がたったとき、
  formal-type に関する不等式を *type-ineq-lt* に追記する
  (actual-type の下界で formal-type を下から抑えられる)  
  "
  (assert (or (typevarp formal-type) (group-or-classp formal-type)) "formulate-formal(): invalid formal type")
  (assert (or (typevarp actual-type) (group-or-classp actual-type)) "formulate-formal(): invalid actual type")  
  (when (and (typevarp formal-type)
              (or (group-or-classp actual-type) (assoc actual-type *type-ineq-gt*)))
    (let* ((lower
              (if (group-or-classp actual-type)
                actual-type
                (cadr (assoc actual-type *type-ineq-gt*))))
            (ineq-pair
              (list formal-type lower)))
      (setq *type-ineq-gt* (unify-ineq ineq-pair *type-ineq-gt* :lt nil)))))


;;; infer-call の補助関数
(defun keysym-eq (x y)
  "
  (keysym-eq :sym 'sym) => t
  関数定義時は &key (sym val) という形であり、関数型は (sym . T) という形で登録されている
  関数コール時には :sym arg という形でくるためそのミスマッチを解決するためのもの
  "
  (and (symbolp x) (symbolp y) (equal (string x) (string y))))


;;; 関数呼び出し（fenv にその関数型があることは保障されている）
(defun infer-call (sexp varenv fenv)
  ;; (format t "call ~a~%" (car sexp))
  (let* ((fn (car sexp))
         (body (cdr sexp))  ; (foo (bar a b) c :key t) なら ((bar a b) c :key t)
         (ftype (getenv fn fenv))
         (formal-params-list (car ftype))  ; 登録された関数仮引数の型のリスト
         (formal-ret (cdr ftype))          ; 登録された関数戻り値の型
         (ret-memo nil))  ; 呼び出し結果の型。formal-ret の型変数のコピー or 対応した実引数型（関数が全称型）
    ;; 仮引数の型の羅列と、実引数の S 式の羅列を対応づける
    ;; 実引数側はキーワードの処理を行うため (mapcar #'infer body) として型の羅列にしてはいけない
    (while body
      (let* ((actual-sexp (pop body))
             (formal-top (car formal-params-list))
             ; 注目している実引数の S 式がキーワード引数のためのものか
             ; actual-sexp がキーワードであり、引数結合のためのリストはキーワードのやつしか余っていなく、マッチングするキーワードが存在するか
             (is-key-arg 
                (and (keywordp actual-sexp)
                     (consp formal-top)
                     ; rest よりキーワードの結合が優先される模様。独自プロトコルのrepeatも同じ扱いとする。
                     ; optional が残っていた場合はキーワード引数として扱わぬ
                     (not (eq (car formal-top) 'optional))
                     (position-if #'(lambda (x) (and (consp x) (keysym-eq (car x) actual-sexp)))
                                  formal-params-list)))
             ; 注目している実引数が &rest 分に対応するものか
             (is-rest
                (and (not is-key-arg)
                     (consp formal-top)
                     (eq (car formal-top) 'rest))))
        ;; rest に吸われる場合は不等式がたたない
        ;; それ以外の場合、引数結合させるときの仮引数の型、実引数の型を求める。
        (unless is-rest
            (let ((formal-type
                    (cond
                        ; (keysym . a) の a を得る
                        (is-key-arg
                          (cdr (elt formal-params-list 
                                    (position-if #'(lambda (x) (and (consp x) (keysym-eq (car x) actual-sexp))) formal-params-list))))
                        ; (repeat . a) (formal-params-list から pop してはだめ)
                        ((and (consp formal-top) (eq (car formal-top) 'repeat))
                          (cdr formal-top))
                        ; (optional . a)
                        ((and (consp formal-top) (eq (car formal-top) 'optional))
                          (cdr (pop formal-params-list)))
                        ; class or group or typevar
                        (t
                          (pop formal-params-list))))
                  (actual-type
                    (if is-key-arg
                      (infer (pop body) varenv fenv) ; ( ... :keysym sexp ...) の sexp をとるために pop
                      (infer actual-sexp varenv fenv))))
              ; 制約立式
              ; 実引数が関数の場合 infer-unary に流れた時、今の実装では変数を探すだろう。なかったら any-class を返してくれるので制約がたたないので嬉しい
              ; !別のやつを見つけてしまったり、関数型を見つけてしまうと formal-type に compiled-code, actual-type に ((arg) . ret) が入ってまずい!
              (formulate-actual formal-type actual-type)
              ; 注目している仮引数の型変数と戻り値の型変数が等しい場合、実引数の型を戻り値の型としてセット（全称型）
              (when (and (typevarp formal-type) (typevarp formal-ret) (eq formal-type formal-ret))
                (setq ret-memo actual-type))))))   ; end of while
    ; 関数が全称型でなく、formal-ret が型変数だった場合、同様な制約を持つ型変数を生成して返す
    (cond ((not (null ret-memo)) ret-memo)
          ((not (typevarp formal-ret)) formal-ret)
          (t
            (let ((upper (cadr (assoc formal-ret *type-ineq-lt*)))
                  (lower (cadr (assoc formal-ret *type-ineq-gt*)))
                  (fresh-variable (sym)))
              (when (not (null upper))
                (setq *type-ineq-lt* (unify-ineq (list fresh-variable upper) *type-ineq-lt* :lt t)))
              (when (not (null lower))
                (setq *type-ineq-gt* (unify-ineq (list fresh-variable lower) *type-ineq-gt* :lt nil)))
              fresh-variable)))))



;;; prog1, progn
(defun infer-prog (sexp varenv fenv)
  (let ((ret nil) ; 最初もしくは最後の S 式の型
        (i 0)
        (head (car sexp))
        (body (cdr sexp)))
    (dolist (elm body)
      (let ((inner-ret (infer elm varenv fenv)))
        (cond 
          ((and (eq head 'prog1) (= i 0)) (setq ret inner-ret))
          ((and (eq head 'progn) (= i (1- (length body)))) (setq ret inner-ret))))
      (incf i))
    ret))



;;; quote
(defun infer-quote (sexp varenv fenv)
  (assert (= (length sexp) 2) "infer-quote(): length of sexp != 2")
  (let ((tail (cadr sexp)))
    (if (consp tail)
      ; '(1 2) など。(quote (1 2 3)) => cons
      cons
      ; 'x => symbol, 'nil => nil-class, ...
      ; x を型環境下で探すわけではないので infer-unary には投げられぬ
      (cond 
        ((null tail) nil-class)
        ((eq tail 't) t-class)
        ((integerp tail) int)
        ((floatp tail) float)
        ((numberp tail) rational)
        ((symbolp tail) symbol)
        (t (class tail))))))



;;; setq, setf, defvar, defparameter, defconstant
;;; 共通部分がほとんどなのでまとめた
(defun infer-set (sexp varenv fenv)
  (assert (= (length sexp) 3) "infer-set(): length of sexp != 3")
  (let* ((head (car sexp))
         (mid (cadr sexp)) ; setf のときは (aref ...) のような list のときがある。そこでも制約を立てられる
         (tail (caddr sexp))
         (ret (infer tail varenv fenv)))
    (assert (or (eq head 'setf) (symbolp mid)) "infer-set(): symbol expected")
    (if (consp mid)
      ; setf でセッター利用のときは推論できるところを推論
      (infer mid varenv fenv)
      ; それ以外はシンボルに型をセット。すでに具象型が入っていたら何もしない。型変数なら上界として制約を追加する。
      ; 新規定義だったら ret が nil-class の場合、型変数を割り当て、それ以外なら ret の型を割り当てる
      (let ((prev-type (getenv mid varenv)))
        (cond ((group-or-classp prev-type)
                 nil)
              ((and (typevarp prev-type) (typevarp ret) (null (assoc ret *type-ineq-lt*))) ; 上界がなかった
                 nil)
              ((and (typevarp prev-type) (typevarp ret))
                ; 下で (list prev=type (...)) の prev-type のところが mid となっていたが間違いのはず？（ineq-lt は型変数に対するものなので）
                 (setq *type-ineq-lt* (unify-ineq (list prev-type (cadr (assoc ret *type-ineq-lt*))) *type-ineq-lt* :lt t)))
              ((typevarp prev-type)
                 (setq *type-ineq-lt* (unify-ineq (list prev-type ret) *type-ineq-lt* :lt t)))
              ((equal ret nil-class) (setenv mid varenv (sym)))
              (t (setenv mid varenv ret)))))
    ret))



;;; infer-let, argparse でヘビーに使用する補助関数
(defun init-typing (sexp varenv fenv &key var target-env)
  "
  sexp は「新しく導入された」変数 var の初期値となる S 式であるとする
  (setq などではすでに型情報があることがあるため、使用できない)
  環境下で sexp の型を評価し、nil-class となった場合、var に型変数を割りあて target-env にその深さで登録する。
  それ以外の型ないし型変数で定まった場合、その型を target-env にその深さで登録する。
  (例外)
  初期値特有の問題で整数値で初期化しようと小数値で初期化しようと、数値なら OK という気持ちのことが多い。
  初期値特有の問題で t で初期化しているときは bool なら OK という気持ちのことが多い。適宜広げてやる。
  登録した初期値の型を返す
  "
  (assert (symbolp var) "init-typing(): var should be a symbol")
  (let* ((init-type 
            (infer sexp varenv fenv))
         (register-type
            (cond
              ((equal init-type nil-class) (sym))
              ((equal init-type t-class) bool)
              ((member init-type (list int float rational)) number)
              (t init-type))
            ))
    ; here でないとだめ
    (setenv-here var target-env register-type)
    register-type))



;;; let, let*
(defun infer-let (sexp varenv fenv)
  (let ((head (car sexp))
        (bind (cadr sexp))
        (body (cddr sexp))  ; (sexp sexp ...). 暗黙の progn なので caddr ではない
        (new-varenv (make-env :outer varenv)))
    ;; 各ローカル変数と型のバインドを型環境へ追加
    (dolist (elm bind)
      (if (consp elm)
        ; 初期値あり
        (init-typing (cadr elm) (if (eq head 'let) varenv new-varenv) fenv :var (car elm) :target-env new-varenv)
        ; 初期値なし here でないとだめ
        (setenv-here elm new-varenv (sym))))
    (infer `(progn ,@body) new-varenv fenv)))



;;; infer-defun, infer-flet の補助関数
(defun argparse (arg-lst varenv fenv parent-varenv)
  "
  1. 関数定義の引数部分を解析し、関数型を構築する
  (a:a0 b:a1 &optional c:a2 d:a3 &rest ls:cons &key e:a4 f:a5 &aux g:a6 h:a7) 
  --> (a0 a1 (optional . a2) (optional . a3) (rest) (e . a3) (f . a4))
  (&aux は関数内 let の略記であり、関数型には現れない)

  2. 初期値がない or あっても nil ならその引数には型変数をあてる。初期値が定まっているならその型をあてる。
  上記のようにして仮引数と型が登録されたローカル変数環境を、parent-env を親にして構築する

  3. 上記関数の引数型を表すリスト、ローカル変数環境を dot pair にして返す
  （関数の戻り値については担当しない。あくまでも引数のみ）

  arg-lst: 関数定義の引数部分のリスト
  varenv, fenv: 初期値指定部で大域関数を使用することなどがある。初期値に型をつけるために必要
  parent-env: 親に設定して欲しい環境
  "
  (let ((arg-sym-lst nil)
        (child-env (make-env :outer parent-varenv)))
    (if (null arg-lst)
      ; 無引数関数の場合
      (push nil-class arg-sym-lst)
      ; 引数があるなら解析、同時にバインドも型環境へ追加。
      ; (a b &optional c d &rest ls &key e f &aux g h) の順なので後ろから処理
      ; [prev:] を処理済みであるとして経過を記録していく
      (let ((prev (length arg-lst)))
        ; &aux g (h 1) => 環境にのみ追加
        (when (find '&aux arg-lst)
          (let* ((&aux-pos (position '&aux arg-lst))
                 (auxarg-lst (subseq arg-lst (1+ &aux-pos) prev)))
            (dolist (elm auxarg-lst)
              (let* ((var (if (consp elm) (car elm) elm))
                     (init-sexp (if (consp elm) (cadr elm) nil))
                     (init-type (init-typing init-sexp varenv fenv :var var :target-env child-env)))))
            (setq prev &aux-pos))
          )
        ; &key e (f 1) => (e . ?a0) (f . int) が追加される
        (when (find '&key arg-lst)
          (let* ((&key-pos (position '&key arg-lst))
                 (keyarg-lst (subseq arg-lst (1+ &key-pos) prev))
                 (keyarg-sym-lst nil))  ; buffer
            (dolist (elm keyarg-lst)
              (let* ((var (if (consp elm) (car elm) elm))
                     (init-sexp (if (consp elm) (cadr elm) nil))
                     (init-type (init-typing init-sexp varenv fenv :var var :target-env child-env)))
                (push-cons-al (cons var init-type) keyarg-sym-lst)))
            (push-al-al (reverse keyarg-sym-lst) arg-sym-lst)   ; ((f . int) (e . ?a0)) になっているので反転
            (setq prev &key-pos))
          )
        ; &rest ls => (rest) が追加される
        (when (find '&rest arg-lst)
          (let* ((&rest-pos (position '&rest arg-lst))
                 (restvar (elt arg-lst (1+ &rest-pos))))  ; &rest ls の ls
            (setenv-here restvar child-env cons)  ; here でないとだめ
            (push-cons-al '(rest) arg-sym-lst)
            (setq prev &rest-pos))
          )
        ; &optional c (d 1) => (optional . ?a1) (optional . int) が追加される
        (when (find '&optional arg-lst)
          (let* ((&op-pos (position '&optional arg-lst))
                 (oparg-lst (subseq arg-lst (1+ &op-pos) prev))
                 (oparg-sym-lst nil)) ; buffer
            (dolist (elm oparg-lst)
              (let* ((var (if (consp elm) (car elm) elm))
                     (init-sexp (if (consp elm) (cadr elm) nil))
                     (init-type (init-typing init-sexp varenv fenv :var var :target-env child-env)))
                (push-cons-al (cons 'optional init-type) oparg-sym-lst)))
            (push-al-al (reverse oparg-sym-lst) arg-sym-lst)
            (setq prev &op-pos))
          )
        ; 残りの通常引数
        (let ((normalarg-lst (subseq arg-lst 0 prev))  ; (a b) など
              (normalarg-sym-lst nil))  ; buffer
          (dolist (elm normalarg-lst)
            (assert (and (symbolp elm) (atom elm)) "argparse(): invalid default parameter?")
            (let ((init-type (sym)))
              (setenv-here elm child-env init-type) ; here でないとだめ
              (push init-type normalarg-sym-lst)))
          ; (b a) になっているので反転. (a b ...) と入って欲しいので push-cons-cons              
          (push-cons-cons (reverse normalarg-sym-lst) arg-sym-lst)
          )
        ))  ; end of let
    ; return
    (cons arg-sym-lst child-env)))



;;; defun
(defun infer-defun (sexp varenv fenv)
  ;; (format t "infer-defun: ~A~%" (cadr sexp)) ; debug
  (let* ((fn (cadr sexp))
         (arg-lst (caddr sexp))
         (body (cdddr sexp)) ; (sexp sexp ...). 暗黙の progn なので cadeddr ではない
         (dot-pair (argparse arg-lst varenv fenv varenv))
         (arg-type-lst (car dot-pair))
         (new-varenv (cdr dot-pair))
         (temporal-ret-type (sym))  ; 再帰関数などの都合上とりあえず置く
         (fn-type (cons arg-type-lst temporal-ret-type)))
    (setenv fn fenv fn-type)
    (let ((ret-type (infer `(progn ,@body) new-varenv fenv)))
      ; 実際の型で戻り値型を置換
      (setf (cdr (getenv fn fenv)) ret-type)
      symbol)))



;;; flet, labels
;; (flet ((f (arg) sexp sexp ...)
;;        (g (arg) sexp sexp ...))
;;   sexp sexp ...)
(defun infer-flet (sexp varenv fenv)
  (let* ((head (car sexp))
         (bind (cadr sexp))
         (body (cddr sexp))  ; (sexp sexp ...). 暗黙の progn なので caddr ではない
         (new-fenv (make-env :outer fenv)))
    (dolist (elm bind)
      (let* ((fn (car elm))
             (arg-lst (cadr elm))
             (fn-body (cddr elm))
             (dot-pair (argparse arg-lst varenv fenv varenv))
             (arg-type-lst (car dot-pair))
             (new-varenv (cdr dot-pair))
             (temporal-ret-type (sym))
             (fn-type (cons arg-type-lst temporal-ret-type)))
        ; here でないとだめ
        (setenv-here fn new-fenv fn-type)
        ; labels のときのみ再帰が可能
        (let ((ret-type (infer `(progn ,@fn-body) new-varenv (if (eq head 'flet) fenv new-fenv))))
          ; 実際の形で戻り値型を置換
          (setf (cdr (getenv fn new-fenv)) ret-type))
        ))
    (infer `(progn ,@body) varenv new-fenv)))


;; function, #'
;; #'f or #'(lambda (arg) sexp)
;; - こいつが compiled-code を返すのが重要。
;;   特に引数の関数型情報を活かせない mapcar などは第一引数の型を compiled-code として set-ftype している。
;;   関数コール時の引数結合の際の型の比較で片方が具象型 compiled-code、片方が (S . T) のような表現だと困る
;;   関数型情報を活かすことでより絞れる apply などの高階関数は個々に対応する
;; - EusLisp において高階関数の関数引数はラムダ式以外シンボル表記でも許されるため、そう書かれるとバグる。
;;   (mapcar '1+ '(1 2 3)) と書かれると現状 compiled-code :> symbol なる比較が入る
(defun infer-function (sexp varenv fenv)
  (assert (= (length sexp) 2) "infer-function(): length of sexp != 2")
  (assert (or (symbolp (cadr sexp)) (consp (cadr sexp))) "infer-function(): invalid lambda form")
  compiled-code)


;; while, unless
(defun infer-while (sexp varenv fenv)
  (let ((test (cadr sexp))
        (body (cddr sexp))) ; 暗黙の progn
    (infer test varenv fenv)
    (infer `(progn ,@body) varenv fenv)))


;; loop
(defun infer-loop (sexp varenv fenv)
  (let ((body (cdr sexp)))  ; 暗黙の progn
    (infer `(progn ,@body) varenv fenv)))


;; dotimes, dolist
(defun infer-do (sexp varenv fenv)
  (let* ((head (car sexp))
         (args (cadr sexp))
         (left (car args))  ; 新たに導入される変数
         (right (cadr args))  ; こちらは評価される
         (body (cddr sexp))  ; 暗黙の progn
         (new-varenv (make-env :outer varenv)))
    (assert (= (length args) 2) "infer-do(): invalid num of args")
    (assert (symbolp left) "infer-do(): symbol expected")
    ; 辞書の構築と制約の立式
    (if (eq head 'dotimes)
      (progn
        (setenv-here left new-varenv int)
        (formulate-actual int (infer right varenv fenv)))
      (progn
        (setenv-here left new-varenv (sym)) ; 型は未知
        (formulate-actual cons (infer right varenv fenv))))
    (infer `(progn ,@body) new-varenv fenv)))


;; block, catch
;; (block tag form*)
(defun infer-block (sexp varenv fenv)
  (let ((head (car sexp))
        (tag (cadr sexp))
        (body (cddr sexp))) ; 暗黙の progn
    (when (eq head 'catch) (infer tag varenv fenv)) ; catch は tag を評価
    (infer `(progn ,@body) varenv fenv)))


;; return-from, throw
;; (return-from tag val)
(defun infer-return-from (sexp varenv fenv)
  (assert (= (length sexp) 3) "infer-return(): length of sexp != 3")
  (let ((head (car sexp))
        (tag (cadr sexp))
        (val (caddr sexp)))
    (when (eq head 'throw) (infer tag varenv fenv))  ; throw は tag を評価
    (infer val varenv fenv)))


;; return
(defun infer-return (sexp varenv fenv)
  (assert (= (length sexp) 2) "infer-return(): length of sexp != 2")
  (infer-return-from `(return-from nil ,(cadr sexp)) varenv fenv))


;; push, pushnew
;; (push item place) or (pushenw item place :test :test-not :key)
(defun infer-push (sexp varenv fenv)
  (assert (>= (length sexp) 3) "infer-push(): length of sexp < 3")
  (let ((item (cadr sexp))
        (place (caddr sexp))) ; 最後はセッター利用でもよい
    (infer item varenv fenv)   ; ここでも制約立てられる
    (if (symbolp place)
      (formulate-actual cons (getenv place varenv))  ; cons :> 変数なる不等式がたつ
      (infer place varenv fenv)) ; ここでも制約立てられる
    ; 戻り値型は常に cons
    cons))


;; pop
(defun infer-pop (sexp varenv fenv)
  (assert (= (length sexp) 2) "infer-pop(): length of sexp != 2")
  (let ((tail (cadr sexp)))
    (if (symbolp tail)
      (formulate-actual cons (getenv tail varenv))
      (infer tail varenv fenv))
    ; 戻り値型はわからん
    any-class))


;; incf, decf
(defun infer-incf (sexp varenv fenv)
  (assert (= (length sexp) 2) "infer-incf(): length of sexp != 2")
  (let ((tail (cadr sexp))) ; セッター利用でもよい
    (if (symbolp tail)
      (formulate-actual int (getenv tail varenv)) ; int :> 変数なる不等式がたつ
      (infer tail varenv fenv))
    ; 戻り値型は int
    int))


;; with-output-to-string
;; (with-output-to-string (str) sexp sexp...)
(defun infer-output-string (sexp varenv fenv)
  (let* ((var (caadr sexp))
         (body (cddr sexp)) ; 暗黙の progn
         (new-varenv (make-env :outer varenv)))
    ; 引数の型を stream として束縛
    (setenv-here var new-varenv stream)
    (infer `(progn ,@body) new-varenv fenv)
    ; 戻り値型は string
    string))



;; funcall, apply
;; これまで通り (set-ftype funcall any-class compiled-code (rest)) としてもいいのだけど、頑張るともう少し情報が得られる
;; これまでは compiled-code :> arg1 をして、any-class を返していた
;; 少し進めて、arg1 が
;; #'(lambda ...) のとき arg1 が compiled-code となるのは自明。立式チェックはしない。
;; any-class を返せばよい
;; #'foo or 'foo のとき arg1 が compiled-code となるのは自明 (quote はシンボルではなく関数と解釈されるのだろう)。 立式チェックはしない。
;; foo の関数型が存在するはず。探して戻り値型を返す
;; foo のときシンボルの型を探し、compiled-code :> type of foo で立式できる。((defun f (foo) (funcall foo arg) などを考えよ)
;; any-class を返す
;; とする。真ん中のパターンのとき、式に型をつけることができた！
(defun infer-funcall (sexp varenv fenv)
  (let ((fn-section (cadr sexp))
        (res (cddr sexp)))
    (assert (or (and (symbolp fn-section) (not (null (getenv fn-section varenv))))  ; unbound であることはない
                (and (consp fn-section) (member (car fn-section) '(function quote))))
      "infer-funcall(): invalid function form?")
    ; 関数ではない残りの部分も評価はしておける
    ; 関数適用などの S 式が来ていたりしたら制約が立って儲けもの
    (dolist (elm res) (infer elm varenv fenv))
    ; 立式できる場合は立式
    (when (symbolp fn-section)
      (formulate-actual compiled-code (getenv fn-section varenv)))
    ; 戻り値型
    (if (or (symbolp fn-section) (consp (cadr fn-section)))
      ; lambda or ローカル変数
      any-class
      (progn
        (assert (not (null (getenv (cadr fn-section) fenv))) "infer-funcall(): no function?")
        (cdr (getenv (cadr fn-section) fenv))))))



;; instance, make-instance
;; (instance class &rest)
(defun infer-make-instance (sexp varenv fenv)
  (let ((mid (cadr sexp))
        (res (cddr sexp)))
    ; 残りの部分もキーワードとか色々くるだろうが評価はしておける
    ; 関数適用などの S 式が来ていたりしたら制約が立って儲けもの
    (dolist (elm res) (infer elm varenv fenv))
    (cond
      ; mid はクラスオブジェクトを表すシンボルのとき。
      ; この判定法だと別の変数にクラスを束縛されたときに生成できない。無理なのでスルーする
      ; (このシンボルは評価値がクラスと結びついているだけで、シンボルの示すクラスはメタクラスということになる
      ; 定義したクラス名そのままのシンボルの時はインタプリタはその名前を知っているので eval すればよい
      ; それ以外は動的に追跡しているなら eval でいいが、今回静的にコードをみているので変数名の型しかわからん
      ; オブジェクトの内容はわからないので諦める)
      ((and (symbolp mid) (classp (eval mid)))
        (eval mid))
      ((or (symbolp mid) (consp mid))
        (formulate-actual metaclass (infer mid varenv fenv))
        any-class)  ; 判定不能
      (t (error "infer-make-instance(): unknown class parameter")))))


;; instantiate
;; (instantiate class &optional size)
(defun infer-instantiate (sexp varenv fenv)
  (assert (<= (length sexp) 3) "infer-instantiate(): length of sexp should be 2 or 3")
  (when (= (length sexp) 3)
    (let ((tail (infer (caddr sexp) varenv fenv)))
      (when (typevarp tail) (formulate-actual int tail))))  ; int :> 変数なる不等式
  (let ((mid (cadr sexp)))
    (cond 
      ; (getenv mid varenv) するとクラスのクラスを尋ねて metaclass がくることに注意。mid のままでよい
      ((and (symbolp mid) (classp (eval mid)))
        (eval mid))
      ((or (symbolp mid) (consp mid))
        (formulate-actual metaclass (infer mid varenv fenv))
        any-class)  ; 判定不能
      (t (error "infer-instantiate(): unknown class parameter")))))


;; coerce
;; (coerce seq result-type)
(defun infer-coerce (sexp varenv fenv)
  (assert (= (length sexp) 3) "infer-coerce(): length of sexp != 3")
  (let ((mid (cadr sexp))
        (tail (caddr sexp)))
    (formulate-actual seq (infer mid varenv fenv))
    (cond 
      ((and (symbolp tail) (classp (eval tail)))
        (eval tail))
      ((or (symbolp tail) (consp tail))
        (formulate-actual metaclass (infer tail varenv fenv))
        any-class)  ; 判定不能
      (t (error "infer-coerce(): unknown class parameter")))))


;; concatenate
;; (concatenate result-type seq*)
(defun infer-concatenate (sexp varenv fenv)
  (let ((mid (cadr sexp))
        (res (cddr sexp)))
    (dolist (elm res) ; result-type 以降の全ての実引数について立式
      (formulate-actual seq (infer elm varenv fenv)))
    (cond 
      ((and (symbolp mid) (classp (eval mid)))
        (eval mid))
      ((consp mid)
        (formulate-actual metaclass (infer mid varenv fenv))
        any-class)  ; 判定不能
      (t (error "infer-concatenate(): unknown class parameter")))))


;; map
;; (map result-type function seq*)
(defun infer-map (sexp varenv fenv)
  (let ((two (cadr sexp))
        (three (caddr sexp))
        (res (cdddr sexp)))
    (formulate-actual compiled-code three)
    (dolist (elm res) ; function 以降の全ての実引数について立式
      (formulate-actual seq elm))
    (cond 
      ((and (symbolp two) (classp (eval two)))
        (eval two))
      ((consp two)
        (formulate-actual metaclass (infer two varenv fenv))
        any-class)  ; 判定不能
      (t (error "infer-map(): unknown class parameter")))))



;; and, or, if, when, unless, cond, case
(defun infer-flow (sexp varenv fenv)
  "
  条件分岐の部分の推論を行う。実験のため *flow-mode* で挙動の制御を行う。
  nil のとき
  確実に評価される最初の test 部分のみ見て、戻り値の型は any とする
  t のとき
  最初の test は必ず見る。test 部分全てにクラス判定ディスパッチがないと予測できる場合、
  全ての test と condition body を見る。
  戻り値の型は型変数をあて、上界を全ての condition body の Union とする
  "
  (if *flow-mode*
    (infer-flow-precise sexp varenv fenv)
    (infer-flow-coarse sexp varenv fenv)))


(defun infer-flow-coarse (sexp varenv fenv)
  (let* ((head (car sexp))
         (test (if (eq head 'cond) (caadr sexp) (cadr sexp))))
    (infer test varenv fenv)
    any-class))


;;; infer_flow_helper.l の補助関数に頼る
(defun infer-flow-precise (sexp varenv fenv)
  (let ((head (car sexp)))
    (cond
      ((member head '(and or)) 
         (infer-and sexp varenv fenv))
      ((and (eq head 'if) (= (length sexp) 4))
         (infer-if-full sexp varenv fenv))
      ((member head '(if when unless))
         (infer-when sexp varenv fenv))
      ((eq head 'cond)
         (infer-cond sexp varenv fenv))
      ((eq head 'case)
         (infer-case sexp varenv fenv))
      (t (error "infer-flow-precise(): not a conditional sexp")))))



(defun calc-return-union (ret-type-lst)
  "
  condition body の戻り値を集めてその Union を返す際の補助関数
  ret-type-lst は各条件分岐での戻り値の型を集めたものである
  それらが全て具象型の場合その Union をとった結果を表すクラスないし group のインスタンスを返す
  型変数が一つでも含まれていたら、それぞれの上界を集め、それらの Union を上界とする型変数を生成して返す
  "
  (assert (every #'(lambda (x) (or (group-or-classp x) (typevarp x))) ret-type-lst) "calc-return-union(): invalid ret type")
  (if (some #'typevarp ret-type-lst)
    ; 型変数があった
    (let ((upper
            ; 型変数を全て上界で置換したリストを作成して reduce
            (reduce #'class-union
                    (mapcar #'(lambda (x) (if (typevarp x) 
                                            (if (assoc x *type-ineq-lt*)
                                              (cadr (assoc x *type-ineq-lt*))
                                              object) ; 上界なし
                                            x))
                            ret-type-lst)))
          (typevar (sym)))
      (push-cons-al (list typevar upper) *type-ineq-lt*)
      typevar)
    ; 全て具象型
    (reduce #'class-union ret-type-lst)))


;; and, or
(defun infer-and (sexp varenv fenv)
  (let ((body (cdr sexp)))  ; 暗黙の progn
    (if (dynamic-dispatch-p body) ; and, or は全てが test である
      (progn
        (infer (car body) varenv fenv)  ; 最初の test は必ず見る
        any-class)
      (let ((ret-type-lst nil))
        (dolist (elm body)  ; condition 部分全部見て OK
          (push (infer elm varenv fenv) ret-type-lst))
        (calc-return-union ret-type-lst)))))  ; 戻り値の Union をとる


;; if (else まであり)
(defun infer-if-full (sexp varenv fenv)
  (let ((test (cadr sexp))
        (then (caddr sexp))
        (else (cadddr sexp)))
    (infer test varenv fenv)
    (if (dynamic-dispatch-p test)
      any-class
      (calc-return-union (list (infer then varenv fenv) (infer else varenv fenv))))))


;; if (then のみ), when, unless
(defun infer-when (sexp varenv fenv)
  (let ((test (cadr sexp))
        (body (cddr sexp))) ; 暗黙の progn. then のみ if の場合 ((then sexp)) という形
    (infer test varenv fenv)
    (if (dynamic-dispatch-p test)
      any-class
      (infer `(progn ,@body) varenv fenv))))


;; cond
;; (cond (test sexp) (...) (...))
(defun infer-cond (sexp varenv fenv)
  (let ((test-body-pairs (cdr sexp)))
    (if (dynamic-dispatch-p (mapcar #'car test-body-pairs))
      (progn 
        (infer (caar test-body-pairs) varenv fenv)  ; 最初の test は見る
        any-class)
      (let ((ret-type-lst nil))
        (dolist (pair test-body-pairs)
          (infer (car pair) varenv fenv)
          (push (infer (cadr pair) varenv fenv) ret-type-lst))
        (calc-return-union ret-type-lst)))))



;; case
;; (case val ((not eval-ed list) sexp) (val-is-also-ok sexp) (...))
(defun infer-case (sexp varenv fenv)
  (let ((val (cadr sexp))
        (label-body-pairs (cddr sexp)))   ; test はない。eq による比較のみ
    (infer val varenv fenv)
    (let ((ret-type-lst nil))
      ; (label body-sexp1 ...) と書ける。これも暗黙の progn
      (dolist (body (mapcar #'cdr label-body-pairs))
        (push (infer `(progn ,@body) varenv fenv) ret-type-lst))
      (calc-return-union ret-type-lst))))



;; send, send*
(defun infer-receiver (sexp varenv fenv)
  "
  レシーバータイプの推論を行う。実験のため *receiver-mode* で挙動の制御を行う。
  nil のとき、何もせず戻り値の型は any とする
  t のとき、(send x :method arg) の :method から x を絞り込む。戻り値の型は any とする
  "
  (if *receiver-mode*
    (infer-receiver-precise sexp varenv fenv)
    (infer-receiver-coarse sexp varenv fenv)))


(defun infer-receiver-coarse (sexp varenv fenv)
  any-class)


;;; infer_receiver_helper.l の補助関数に頼る
(defun infer-receiver-precise (sexp varenv fenv)
  (let* ((receiver (cadr sexp))
         (actual-type (infer receiver varenv fenv))
         (selector (caddr sexp))
         (formal-type (method-to-class-or-group selector)))
    (formulate-actual formal-type actual-type)
    any-class))


