(provide :inference-eusfunc)
(unless (find-package "INFERENCE-EUSFUNC") (make-package "INFERENCE-EUSFUNC"))
(in-package "INFERENCE-EUSFUNC")


(require ":BUGFIXED-HASH" "bugfixed_hash.l")
(require "builtin_type_data.l")



;;; Euslisp における唯一のオブジェクトでないやつら。処理の都合上あらかじめ擬似クラスを作成しておく
(defclass number-class :super object)
(defclass integer-class :super number-class)
(defclass float-class :super number-class)
(defclass rational-class :super number-class)
; cons でもあり symbol でもある。しかしUnion(cons, symbol)「とは異なる」(集合論的にはIntersection(cons, symbol)なのだと思う)。
; ある変数にnil-classが入っており、さらに他の方程式でconsなる制約が入ってもconsに更新はするべきでない。nil-classのまま。symbolの制約についても然り。
; 制約の適用を現在の型の集合と制約の示す集合の intersection をとる作業だと考えると、Intersection(cons, symbol)という理解がしっくりくる。
(defclass nil-class :super object)
(defclass sequence-class :super object)    ; Union(cons, vector)
(defclass pathlike-class :super object)    ; Union(pathname, string)
(defclass symbollike-class :super object)    ; Union(symbol, string)
(defclass packagelike-class :super object)    ; Union(package, string)


(defun union-classp (cls)
  (or (eq cls sequence-class) (eq cls pathlike-class) (eq cls symbollike-class) (eq cls packagelike-class)))


(defun union-to-component (cls)
  (assert (union-classp cls))
  (cond ((eq cls sequence-class) (list cons vector))
        ((eq cls pathlike-class) (list pathname string))
        ((eq cls symbollike-class) (list symbol string))
        ((eq cls packagelike-class) (list package string))))


;;; 連想リストを連想リストへ破壊的に追加するマクロ
;;; nil を push しても変化しない
(defmacro push-al-al (from-alist to-alist)
  `(setq ,to-alist (append ,from-alist ,to-alist)))

;;; ドットリストを連想リストへ破壊的に追加するマクロ
;;; nil を push しても変化しない ( equation (alist) が１つも立たなかった時、それらをまとめた equations (alist) の表記は nil であるべき。)
(defmacro push-cons-al (from-cons to-alist)
  `(unless (null ,from-cons) (setq ,to-alist (cons ,from-cons ,to-alist))))



;;; 組み込み関数の集合
;;; (! * + - / /= 1+ 1-  ... ) の各要素をキーに、値はtで一律に hash-table に登録されている
(defparameter *builtin-functions* (BUGFIXED-HASH::make-hash-table :size 2560))

(dolist (elm (functions "" "LISP"))
  (setf (BUGFIXED-HASH::gethash elm *builtin-functions*) t))


;;; x (symbol) が組み込み関数か調べる
(defun builtin-p (x)
  (BUGFIXED-HASH::gethash x *builtin-functions*))


;;; ユーザー関数の集合
;;; defun されている関数シンボルをキーに、値はtで一律に hash-table に登録している
(defparameter *user-functions* (BUGFIXED-HASH::make-hash-table))


;;; x (symbol) がユーザー関数か調べる
(defun user-p (x)
  (BUGFIXED-HASH::gethash x *user-functions*))



;;;;;;;;;;;;;;;;;;;;
;;; 不変な型環境 ;;;
;;;;;;;;;;;;;;;;;;;;


;;; 不変な組み込み変数の型環境 (スコープの関係上関数の型環境とわけている)。この値で適宜 *global-type-env* を初期化する。
;;; 変数名 . 型 なる association list
;;; (hoge . <int>) の形で記述。型方程式と似ているが、左辺値の変数が右辺値の型であるということを言っているだけ。(もちろん (hoge . α) のように型変数で型が定まっていることもあるが)
(defparameter *builtin-type-env* nil)

(dolist (elm (variables "" "LISP"))
    (push-cons-al (cons elm (class (eval elm))) *builtin-type-env*))    ; eval が重要。そうでないと全部シンボルになる
  

;;; 型環境から指定のシンボルの型を取得 (関数の型環境かそれ以外の型環境かは適切に指示しよう)
(defun get-type (sym env)
  (cdr (assoc sym env)))


;;; 不変な組み込み関数の型環境 (スコープの関係上変数の型環境とわけている)。この値で適宜 *global-ftypes* を初期化する。
;;; 関数名 . 型 なる association list 
;;; 関数の型は (ret-type arg-type) の形で記述。arg-type はリストで (τ1 τ2 (optional . τ3) (optional . τ4) (rest) (key1 . τ5) (key2 . τ6)) の形で記述
(defparameter *builtin-ftypes* nil)

;;; 1000個以上あるビルトイン関数の型情報の登録をラクに行うためのマクロ。以下のように列挙するだけで、クオートなしに登録可能。
;;; e.g. (set-ftype somefunc ret-type τ1 τ2 (optional . τ3) (optional . τ4) (rest) (key1 . τ5) (key2 . τ6))
(defmacro set-ftype (fn ret-type &rest arg-type)
  `(push-cons-al
     (cons ',fn (list ,ret-type (mapcar #'(lambda (x) (cond 
                                                        ((and (consp x) (= (length x) 2)) (cons (car x) (eval (car (cdr x)))))    ; (optional . T) or (repeat . T) or (key . T)
                                                        ((consp x) x)    ; (rest)
                                                        (t (eval x))))    ; ordinary symbol notation of a class T
                                        ',arg-type)))
     *builtin-ftypes*))

;;; 組み込み関数の型が登録される (defined in buitin_type_data.l)
(eval set-ftype-command-for-builtin-functions)


;;; functionp だとユーザー関数は (defun が eval されていないので)関数判定されない。
;;; 先に user-p buitin-p をしているのは検索効率がO(1)ですむため。(fletなどによるローカル関数含めftypesに全ては定義されているが、こちらはO(n))
(defun my-function-p (sym ftypes)
  (or (user-p sym) (builtin-p sym) (assoc sym ftypes)))




;;;;;;;;;;;;;;;;
;;; 補助関数 ;;;
;;;;;;;;;;;;;;;;

;;; 型環境 type-env 及び ftypes のもとで x (単項) の属する型クラスを特定する。
;;; e.g. (typing nil nil#i(1 2 3)) => #<vectorclass #X561c8ec00290 integer-vector>
;;; e.g. (typing '((x . #<metaclass #X561c8ec02660 vectorclass>)) nil 'x) => #<metaclass #X561c8ec02660 vectorclass>
;;; is-function フラグが t のときは問答無用で関数であると決め打ちしてOK (extract-defun や extract-call 経由の場合必ず t となってくる)
;;; 立ってないのに関数である場合は、何かの関数の引数として関数が渡されるときのみ。その場合は functionp でカバーできている
(defun typing (type-env ftypes x &optional (is-function nil))
  (cond 
    ; symbol のうち variable でないもの
    ((null x) nil-class)    ; nil
    ((and (symbolp x) (eq x 't)) symbol)    ; t
    ; 関数
    ((derivedp x compiled-code) (get-type x ftypes))    ; #'呼び出しは関数
    (is-function (get-type x ftypes))    ; この関数からシンボルがローカル関数なのかローカル変数なのか判定することはできない
    ((functionp x) (get-type x ftypes))    ; 'append とかローカル関数fに対する 'f とか。今の所関数名と同じローカル引数を指定されるとここで判定不能となり関数決め打ちをしてしまう。
    ; variable
    ((symbolp x) (get-type x type-env))
    ; literal のうちオブジェクトでないもの
    ((integerp x) integer-class)
    ((floatp x) float-class)
    ((numberp x) rational-class)
    ; literal の残り。
    (t (class x))))    ; (send (class x) :name) では文字列表記が返ってしまいクラスオブジェクトは返らない


;;; 型変数かどうか判定するのに使用する
(defun variablep (x)
  (and (symbolp x) (not (equal x 't)) (not (equal x nil))))





;;;;;;;;;;;;;;;;;;
;;; 型推論関連 ;;;
;;;;;;;;;;;;;;;;;;

;;; マクロの処理, そのファイル内でのrequireの処理は一旦置いておく
;;; マクロは一旦ロードしてマクロ名を覚えておき、s-exp を処理していく段階でマクロ名がきたら (macroexpand s-exp) を処理するように変えればよいだけ
;;; requireはそのファイルについても infer-file して環境に追加すればよい。パッケージ化されていたらその prefix の処理もせんとだけど
;;; Z3 などの SMT solver に投げるのも手 (不等式の関係性だけ教えればとける)


;;; 型推論をかけるファイルをこのストリームへ繋げる
(defparameter *instream* nil)


;;; グローバルな関数以外の型環境
(defparameter *global-type-env* nil)
(setq *global-type-env* *builtin-type-env*)
;;; グローバルな関数の型環境
(defparameter *global-ftypes* nil)
(setq *global-ftypes* *builtin-ftypes*)


;;; ユーザーインターフェースとして提供する部分
(defun infer-file (fname)
  ;; 制約の alist
  ;; ((number-class . x)) なら x は number-class の subtype であることが要請されるとする。(左辺値が formal parameter であり、右辺値が actual parameter だと考える。-> extract-call)
  (let ((type-equations nil))
    ;; 1 週目はグローバルなユーザー関数のシンボルを *user-functions* に登録したり、割り当てた型変数を *global-ftypes* に登録することなどに使う
    (with-open-file *instream* fname
      (let ((sexp nil))
        (while (setq sexp (read instream nil nil))
          (when (car (sexp) 'defun)
            (let ((fn (car (cdr sexp))))    ; 関数シンボル
              (setf (BUGFIXED-HASH::gethash fn *user-functions*) t)    ; 関数シンボルを集合に登録
              (push-cons-al (cons fn (list (gensym) (gensym)) *global-ftypes*))        ; 関数シンボルに対し (#Sym1 #Sym2) のように型変数を当てて登録 (#Sym2 は(τ1 τ2)といった型のリストとなる)
              ;; (setf (BUGFIXED-HASH::gethash fn *global-ftypes*) (list (gensym) (gensym)))
              ))))
        (close *instream*)
        (setq *instream* nil))
    ;; 2 週目は推論フェーズに使う
    (with-open-file *instream* fname
      (let ((sexp nil))
        (while (setq sexp (read instream nil nil))
          (let* ((dot-lst (extract-eq *global-type-env* *global-ftypes* sexp))    ; (制約 alist . 評価結果の型) が返る
                 (equations (car dot-lst))    ; 制約 alist。 ((#<vectorclass #X561c8ec00290 integer-vector> . α)), nil など。
                 (result-type (cdr dot-lst)))    ; 評価結果の型
              (setq type-equations (append equations type-equations)))))    ; トップレベルでも制約を全て集めて type-equations にまとめる
        (close *instream*)
        (setq *instream* nil))
    ;; 集めた型制約の式 type-equations を解いていく
    (let* ((dot-lst (preprocessing type-equations *global-ftypes*))
           (separated-equations (car dot-lst))
           (user-func-alist (cdr dot-lst))
           (type-var-to-type (solve-type-equations separated-equations user-func-alist))
           (user-func-dictionary (BUGFIXED-HASH::make-hash-table)))
      ;; user-func-alist と type-var-to-type をもとに、型変数のない形でのユーザー関数の型情報を user-func-dictionary に登録する
      ;; なお、この段階で型は「文字列表記に変換する」
      (dolist (fn-type user-func-alist)
        (let* ((fn (car fn-type))
               (result-type (car (cdr fn-type)))
               (arg-type (car (cdr (cdr fn-type))))
               (converted-result-type (convert-to-ordinary-type result-type type-var-to-type))
               (converted-arg-type (convert-to-ordinary-type arg-type type-var-to-type)))
          (setf (BUGFIXED-HASH::gethash fn user-func-dictionary) (list converted-result-type converted-arg-type))))

user-func-dictionary)))


;;; 型環境 (変数、関数) のもとでS式に対し推論を行い、(制約 alist . 評価結果の型) を作って返す
;;; 具体的な処理は補助関数に投げている
(defun extract-eq (type-env ftypes sexp)
  (cond
    ((atom sexp)
      (cons nil (typing type-env ftypes sexp)))
    ((and (consp sexp) (or (eq (car sexp) 'let) (eq (car sexp) 'let*)))
      (extract-let type-env ftypes sexp))
    ((and (consp sexp) (eq (car sexp) 'defun))
      (extract-defun type-env ftypes sexp))
    ((and (consp sexp) (my-function-p (car sexp) ftypes))
      (extract-call type-env ftypes sexp))
  )
)


;;; <let, let*の処理>
;;; e.g. (extract-let *global-type-env* *global-ftypes* '(let ((a 10) b (c "hoge")) c)) 
;;; => (nil . #<vectorclass #X55d0f73b0ba8 string>) 
;;; 制約式は立たず、評価結果は string
;;; 本来の OCaml 式なら let で型変数を当てる必要はないのだが... もちろん型変数を当てた上でその型変数に対する初期値の型の方程式をあててもよい。
;;; 現状親環境を遡れるスコープチェーンをもっていないため setq などでの型指定の情報をどう伝えるかが問題。 (nil で初期化してその後で setq とかはよくある話)
;;; 常に型変数をあてて、(初期値の型 . 型変数) の制約を push しておく。もし型環境内部で setq などをされたら、その制約式 (新しい型 . 型変数) が上がってくる
;;; nil 初期化は nil-class を当ててはならない。この場合の意味論は object 型である。(nil初期化による型の制約が全く無いようにふるまってほしい) (cf. nil-class の制約は最強)
(defun extract-let (type-env ftypes sexp)
  (assert (or (eq (car sexp) 'let) (eq (car sexp) 'let*)) "(let ...) or (let* ...) should be passed for extract-let")
  (let ((head (car sexp))    ; let or let*
        (bind (car (cdr sexp)))
        (body (car (cdr (cdr sexp))))
        (new-type-env type-env)
        (whole-equations nil))
    ;; 各ローカル変数と型のバインドを型環境へ追加。評価の際に集まった制約式を全体の制約式に追加。
    (dolist (elm bind)
      (if (consp elm)
        ; 初期値があるとき
        (let* ((dot-lst (extract-eq (cond ((eq head 'let) type-env) ((eq head 'let*) new-type-env)) ftypes (car (cdr elm))))    ; いわゆる (E1 . τ1)
               (equations (car dot-lst))    ; E1
               (result-type (cdr dot-lst)))    ; τ1
          (push-cons-al (cons (car elm) result-type) new-type-env)
          (push-al-al equations whole-equations))
        ; 初期値がなく nil 初期化を求められているとき
        (push-cons-al (cons elm nil-class) new-type-env)
        ))
    ;; 変数の型が追加登録された環境で body の制約と型を求める
    (let* ((dot-lst-1 (extract-eq new-type-env ftypes body))
           (equations-1 (car dot-lst-1))
           (result-type-1 (cdr dot-lst-1)))
      (push-al-al equations-1 whole-equations)
(cons whole-equations result-type-1))))


;;; <defun の処理>
;;; (progn (setf (BUGFIXED-HASH::gethash 'testfunc *user-functions*) t) (push-cons-al (cons 'testfunc (list (gensym) (gensym))) *global-ftypes*))
;;; (extract-defun *global-type-env* *global-ftypes* '(defun testfunc (a b) (cons a b))) 
;;; => result-type ... (#:SYM1 #:SYM2)
;;;    equations ... 
;;;      equation1 ... (#:SYM2 #:SYM3 #:SYM4)  
;;;      equation2 ... (#<metaclass #X559ad62be060 cons> . #:SYM1)
;;;      equation3 ... ((#<metaclass #X559ad62bd9b8 object> #<metaclass #X559ad62bd9b8 object>) #:SYM3 #:SYM4)
;;; 登録された関数の型変数 (戻り値、引数) と実際の型 (戻り値、引数) の方程式はこの時点で別々の形で連立することが可能なので、別々に立ててあげる
(defun extract-defun (type-env ftypes sexp)
  (assert (eq (car sexp) 'defun) "(defun ...) should be passed for extract-defun")
  (let* ((fn (car (cdr sexp)))
         (registered-fn-type (typing type-env ftypes fn t))    ; is-function flag
         (arg-lst (car (cdr (cdr sexp))))
         (arg-sym-lst nil)
         (body (car (cdr (cdr (cdr sexp)))))
         (new-type-env type-env)
         (whole-equations nil))
    
    (if (null arg-lst)
      ; 無引数関数の場合。arg-sym-lst には (<nil-class>) が登録されるようにしてやる
      (setq arg-sym-lst (list nil-class))
      ; 引数があるなら、引数パラメータを解析してarg-sym-lstをつくる。同時に各引数と型変数(デフォルト値が与えられぬとき)のバインドも型環境へ追加
      ; (a b &optional c d &rest ls &key f g) の順なので後ろから処理していく
      (let ((previous (length arg-lst)))
        ; &key f (g (cons 1 2)) のように指定がなされていた場合、arg-sym-lst には ((f . #:prog11) (g . #<metaclass #X55783839e060 cons)) が入ればよい
        ; new-type-env には (f . #:prog11) と (g . #<metaclass #X55783839e060 cons) が追加されればよい
        (when (find '&key arg-lst)
          (let* ((&key-pos (position '&key arg-lst))
                (key-arg-lst (subseq arg-lst (1+ &key-pos) previous))
                (key-arg-sym-lst nil))
            (dolist (arg key-arg-lst)
              (let ((type-var nil))
                (if (consp arg)
                  ; デフォルト値部分でS式を書くことはもちろん可能。
                  (let* ((dot-lst (extract-eq type-env ftypes (car (cdr arg))))    ; (d 100) のようにくるので 100 の部分を取り出す。
                         (equations (car dot-lst))
                         (result-type (cdr dot-lst)))
                    (push-al-al equations whole-equations)
                    (setq type-var result-type)
                    (push-cons-al (cons (car arg) type-var) key-arg-sym-lst)
                    (push-cons-al (cons (car arg) type-var) new-type-env))
                  ; デフォルト値の指定がないとき
                  (progn 
                    (setq type-var (gensym))
                    (push-cons-al (cons arg type-var) key-arg-sym-lst)
                    (push-cons-al (cons arg type-var) new-type-env)))))
            (push-al-al (reverse key-arg-sym-lst) arg-sym-lst)
            (setq previous &key-pos)))
        ; &rest ls のように指定がなされていた場合、arg-sym-lst には (rest) が入ればよい
        ; new-type-env には (ls . #<metaclass #X55783839e060 cons) が追加されればよい
        (when (find '&rest arg-lst)
          (let* ((&rest-pos (position '&rest arg-lst))
                 (rest-arg (elt arg-lst (1+ &rest-pos))))
            (push-cons-al (cons rest-arg (eval cons)) new-type-env)
            (push-cons-al (list 'rest) arg-sym-lst)
            (setq previous &rest-pos)))
        ; &optional c (d '(1 . 2)) のように指定がなされていた場合、arg-sym-lstには ((optional . #:prog12) (optional . #<metaclass #X55783839e060 cons)) が入ればよい
        ; new-type-env には (c . #:prog12) と (d . #<metaclass #X55783839e060 cons) が追加されればよい
        (when (find '&optional arg-lst)
          (let* ((&optional-pos (position '&optional arg-lst))
                 (optional-arg-lst (subseq arg-lst (1+ &optional-pos) previous))
                 (optional-arg-sym-lst nil))
            (dolist (arg optional-arg-lst)
              (let ((type-var nil))
                (if (consp arg)
                  ; デフォルト値でS式を書くことはもちろん可能。
                  (let* ((dot-lst (extract-eq type-env ftypes (car (cdr arg))))
                         (equations (car dot-lst))
                         (result-type (cdr dot-lst)))
                    (push-al-al equations whole-equations)
                    (setq type-var result-type)
                    (push-cons-al (cons (car arg) type-var) new-type-env))
                  ; デフォルト値の指定がないとき
                  (progn 
                    (setq type-var (gensym))
                    (push-cons-al (cons arg type-var) new-type-env)))
                (push-cons-al (cons 'optional type-var) optional-arg-sym-lst)))
            (push-al-al (reverse optional-arg-sym-lst) arg-sym-lst)    
            (setq previous &optional-pos)))
        ; 残りの普通の引数部分
        (let ((ordinary-arg-lst (subseq arg-lst 0 previous))
              (ordinary-arg-sym-lst nil))
          (dolist (arg ordinary-arg-lst)
            (let ((type-var (gensym)))
              (setq ordinary-arg-sym-lst (cons type-var ordinary-arg-sym-lst))
              (push-cons-al (cons arg type-var) new-type-env)))
          (setq arg-sym-lst (append (reverse ordinary-arg-sym-lst) arg-sym-lst)))))    ; (append '(α β) '((optional x) (rest) (key1 y) (key2 z))) => (α β (optional x) (rest) (key1 y) (key2 z)) 。nconcは第一引数を破壊するので注意

    (let* ((dot-lst-1 (extract-eq new-type-env ftypes body))
           (equations-1 (car dot-lst-1))
           (result-type-1 (cdr dot-lst-1)))
      (push-al-al equations-1 whole-equations)
      ; 以下の２つは不等式ではなく方程式 
      ; (τ . hogefunc-ret-type) のような式もたてなくてはならない (どうせ preprocessing で置換されるけど)
      (push-cons-al (cons (car registered-fn-type) result-type-1) whole-equations)    ; (hogefunc-ret-type . τ) 
      (push-cons-al (cons result-type-1 (car registered-fn-type)) whole-equations)
      (push-cons-al (cons (car (cdr registered-fn-type)) arg-sym-lst) whole-equations)    ; (hogefunc-arg-type . (α β γ ...)) (表記は (hogefunc-arg-type τ α β...)となるけど)
      (push-cons-al (cons arg-sym-lst (car (cdr registered-fn-type))) whole-equations)

;; (print "here is the end of extract-defun") ;kesu
;; (print "whole-equations") ;kesu
;; (print whole-equations) ;kesu
;; (print "ret-type") ;kesu
;; (print registered-fn-type) ;kesu
(cons whole-equations registered-fn-type))))


;;; <関数呼び出しの処理>
;;; (extract-call *global-type-env* *global-ftypes* '(cons 1 2)) 
;;; => equation... 
;;;      equation1 ... ((#<metaclass #X5627788249b8 object> #<metaclass #X5627788249b8 object>) #<metaclass #X5627787f48a8 integer-class> #<metaclass #X5627787f48a8 integer-class>)
;;;    result-type... #<metaclass #X562778825060 cons>
;;; 一般に
;;; (fn e1 e2 e3) に対し、(extract-eq Γ fn) => (E0, τ0), (extract-eq Γ e1) => (E1 . τ1) ... として (E4, α) と返せばよい 
;;; (E4 = Union(E1, ... , E3, (τ0 . (α (τ1 τ2 τ3)))), α = fresh type variable)
;;; 最後の制約式は car-τ0 . (τ1 τ2 τ3) と cdr-τ0 . α の２つにこの時点で分解可能なのでこの形で立式してあげる
;;; となると α などいらぬ。立式しなくてOK
;;; なお、キーワード引数を指定していた場合のために予め :key1 s-exp1 などは (key1 <someType>) という形に変換してやることも考えたが、(厄介なことに)普通の関数の引数としてキーワードシンボルを渡すことも可能。
;;; キーワードがでてきたら symbolだ! と型を確定させたり、続く引数と結合させたりしないようにする。そのまま残し評価を preprocessing まで遅延させる。
(defun extract-call (type-env ftypes sexp)
  (assert (my-function-p (car sexp) ftypes) "(func ...) should be passed for extract-call")
  (let* ((fn (car sexp))
         (registered-fn-type (typing type-env ftypes fn t))    ; is-function flag
         (arg-lst (cdr sexp))
         (given-fn-arg-type nil)
        ;;  (given-fn-result-type (gensym))
         (whole-equations nil))
    ; 無引数関数に対し nil を与えて呼び出したときは (nil-class . nil-class) なる制約式が立つだろう。しかし無引数で呼び出したときはループを回らぬため given-fn-arg-type が (<nil-class>) になってほしい
    (if (null arg-lst)
      (setq given-fn-arg-type (list nil-class))
      (progn 
        (dolist (arg-sexp arg-lst)
          (if (keywordp arg-sexp)
            (setq given-fn-arg-type (cons arg-sexp given-fn-arg-type))    ; ただそのキーワードを追加するのみ
            (let* ((dot-lst (extract-eq type-env ftypes arg-sexp))
                  (equations (car dot-lst))
                  (result-type (cdr dot-lst)))
              (push-al-al equations whole-equations)
              (setq given-fn-arg-type (cons result-type given-fn-arg-type)))))
        (nreverse given-fn-arg-type)))    ; 順序が逆になっているので反転
    ;; (push-cons-al (cons (car registered-fn-type) given-fn-result-type) whole-equations)
    (push-cons-al (cons (car (cdr registered-fn-type)) given-fn-arg-type) whole-equations)
;; (cons whole-equations given-fn-result-type)
(cons whole-equations (car registered-fn-type))
))



;;; and
;;; or
;;; if
;;; when
;;; unless
;;; cond
;;; case
;;; prog1
;;; progn
;;; flet, labels
;;; quote




;;; 型方程式 (不等式) を簡単化
;;; 最初にユーザー関数に当てられた (ret-type arg-type) なる型変数は、すべて関数の引数部にてあてられた型変数とビルトインの型に置き換えることが可能
;;; *global-ftypes* は (少なくともこの関数内では) 変更しない
;;; 型方程式を受け取って適切に整理した新たな型方程式を作成し、別件で (関数名 . (戻り値型 引数型)) が登録された alist を作成し、consにして返すのみ
(defun preprocessing (whole-equations ftypes)
  (let ((converted whole-equations)
        (separated nil)
        (user-func-set nil)
        (user-func-alist nil))
    ;; 最初にユーザー関数に当てられた型変数を含まない形へと置換して整理
    (maphash #'(lambda (key val) (push key user-func-set)) *user-functions*)
    ;; (print "user func set") ;kesu
    ;; (print user-func-set) ;kesu
    (dolist (fn user-func-set)
      (let* ((registered-fn-type (get-type fn ftypes))
             (registered-ret-type (car registered-fn-type))
             (registered-arg-type (car (cdr registered-fn-type)))
             (opponent-ret-type (find-opponent registered-ret-type whole-equations))    ; (hogefunc-ret-type . τ) のように登録されているのだった (extract-defun) 。書き換えの副作用を考えて converted ではなく whole-equationsから検索する必要がある
             (opponent-arg-type (find-opponent registered-arg-type whole-equations)))    ; (hogefunc-arg-type . (α β γ ...)) (表記は (hogefunc-arg-type τ α β...)となるけど) のように登録されているのだった
        ; (subst new old tree) でASTの全要素について置換
        ;; (print fn) ;kesu
        ;; (print registered-ret-type) ;kesu
        ;; (print registered-arg-type) ;kesu
        ;; (print opponent-ret-type) ;kesu
        ;; (print opponent-arg-type) ;kesu
        (push-cons-al (cons fn (list opponent-ret-type opponent-arg-type)) user-func-alist)
        (setq user-func-alist (subst opponent-ret-type registered-ret-type user-func-alist))
        (setq user-func-alist (subst opponent-arg-type registered-arg-type user-func-alist)) 
        (setq converted (subst opponent-ret-type registered-ret-type converted))
        (setq converted (subst opponent-arg-type registered-arg-type converted))))
    ;; 重複する方程式を削除 ('(a . b) '(a . b) といったコンスの比較は equal) (delete-if だと戻り値はよいのだが、なぜかうまく削除されぬ。処理系のバグっぽい)
    (setq converted (remove-if #'(lambda (x) (and (consp x) (equal (car x) (cdr x)))) converted))
    ;; (print "user-func-alist") ;kesu
    ;; (print user-func-alist) ;kesu
    ;; (print "conv") ;kesu
    ;; (dolist (elm converted) (print elm));kesu
    ;; 関数適用の際に立った方程式の場合、適宜 (optional hoge) などの処理を含めて引数結合を行い、(型変数 . 制約) の形の連立方程式へとバラす
    ;; formal, actual 共に cons のときは formal に仮定義が、actual に実引数の型が書かれているはず (extract-call)
    (dolist (equation converted)
      (let ((formal (car equation))
            (actual (cdr equation)))
        (if (and (consp formal) (consp actual))
          ; actual に注目すりゃ引数かキーワード引数のどちらか
          ; extract-call でうまいことしてキーワードはそのまま残している。そのキーワードが &key に対応するものか1引数として与えられたシンボルなのかを判定してやる必要がある
          (while actual 
            (let* ((actual-arg (pop actual))
                   (formal-top (elt formal 0))
                   ; そもそもキーワードであり、引数結合のためのリストはキーワードのやつしか余っていなく、マッチングするキーワードが存在するとき
                   (is-key-arg (and (keywordp actual-arg) 
                                    (and (consp formal-top) (not (eq (car formal-top) 'optional)))    ; rest よりキーワードの結合が優先される模様。独自プロトコルのrepeatも同じ扱いとする。
                                    (position-if #'(lambda (x) (and (consp x) (equal (car x) actual-arg))) formal)))
                   (formal-arg (cond (is-key-arg nil)
                                    ((and (consp formal-top) (eq (car formal-top) 'repeat)) (cdr formal-top))    ; pop しちゃだめ (repeat . α) の α をとる
                                    ((and (consp formal-top) (eq (car formal-top) 'rest)) nil)    ; pop しちゃだめ この引数に関しての方程式はたたない。(&rest ls) の ls にすわれるのみ
                                    ((and (consp formal-top) (eq (car formal-top) 'optional)) (cdr (pop formal)))    ; (optional . α) の α をとる
                                    (t (pop formal))))    ; 型ないし型変数のatomのはず
                   ; actual-arg がキーワード引数のときはその対応する値をもう一度 pop して得る (key . α) の α をとる
                   (actual-key-arg (if is-key-arg (pop actual) nil))
                   (formal-key-arg (if is-key-arg (cdr (elt formal (position-if #'(lambda (x) (and (consp x) (eq (car x) actual-arg))) formal))) nil)))
                (if is-key-arg
                  (push-cons-al (cons formal-key-arg actual-key-arg) separated)
                  (if formal-arg (push-cons-al (cons formal-arg actual-arg) separated)))    ; &rest と結合するパターンは formal-arg が nil になっている。スルーする。
              ))
          ; 片方でも cons でなければ何も処理しない
          (push-cons-al equation separated))))
    ;; この時点で単一の方程式群になっているはず (consが存在せず、左右に１つの型変数ないし型が現れる)。 
    ;; 自明な式 ((<int> . <int>) や (α . α) のようなもの) を取り除いてあげる
    (setq separated (remove-if #'(lambda (x) (eq (car x) (cdr x))) separated))
    ;; 自明なサブタイプ関係も取り除いてあげる ((<number-class> . <integer-class>) のようなもの)
    (setq separated (remove-if #'(lambda (x) (and (classp (car x)) (classp (cdr x)) (my-superclassp (car x) (cdr x)))) separated))

(cons separated user-func-alist)))


;;; 不等式ではなく方程式が立っているような相手を alist から見つける関数
(defun find-opponent (sym alist)
  (let ((possible-opponents nil)
        (opponent nil))
    ; (α . hoge) なる hoge を候補として全て追加
    (dolist (dot-lst alist)
      (if (eq (car dot-lst) sym) (push (cdr dot-lst) possible-opponents)))
    (nreverse possible-opponents)
    (dolist (candidate possible-opponents)
      ; 各候補について全検索し (hoge . β) となっている β を調査。α = β なる β が存在したらその hoge を opponent として登録
      (dolist (dot-lst alist)
        (if (and (eq (car dot-lst) candidate) (eq (cdr dot-lst) sym))
          (push candidate opponent))
      ))
    (nreverse opponent)
    (cond ((null opponent) (error "there is no opponent!"))
          ((> (length opponent) 1) (progn (format t "cannot specify the opponent... choose the first one.") (car opponent)))
          (t (car opponent)))
    ))


;;; subclassp sub super と比較して順序を逆にしていることに注意 (制約式は (型の制約 . 実際の型) という形で立式されているためこちらのほうが書きやすい)
;;; super が sub に対して superclass 関係にあるということは、 sub なる型の引数に super の制約が入っている時 / super なる型の引数に sub の制約が入っている時、この型を sub に更新するということ
;;; nil-class, sequence-class, pathlike-class, symbollike-class, packagelike-class の処理
(defun my-superclassp (super sub)
  (cond ((eq super nil-class) (eq sub nil-class))
        ; super に cons や symbol のスーパークラスが来た時、nil-class という型は制約を満たすことになる。(symbolだがconsでもある)
        ((eq sub nil-class) (or (subclassp symbol super) (subclassp cons super)))
        ; (subclassp sub super) のおかげで sequence-class sequence-class のような同一クラスの比較が t となってくれる
        ((eq super sequence-class) (or (subclassp sub super) (subclassp sub vector) (subclassp sub cons)))
        ((eq super pathlike-class) (or (subclassp sub super) (subclassp sub pathname) (subclassp sub string)))
        ((eq super symbollike-class) (or (subclassp sub super) (subclassp sub symbol) (subclassp sub string)))
        ((eq super packagelike-class) (or (subclassp sub super) (subclassp sub package) (subclassp sub string)))
        (t (subclassp sub super))))



;;; ユーザー関数の型の alist に登録されている各型変数に対し、単項の連立型方程式たちから解を求め、(型変数 . とりうる型) なる alist にして返す
(defun solve-type-equations (equations user-func-alist)
  (let ((type-var-to-type nil)
        (type-var-lst nil)
        (changed t))
    ; 型変数を抽出
    (dolist (elm user-func-alist)
      (setq type-var-lst (append (remove-if-not #'variablep (flatten (cdr elm))) type-var-lst)))
    ; それらに対し object の初期値を与える
    (dolist (type-var type-var-lst)
      (push-cons-al (cons type-var object) type-var-to-type))
    ; object から型の候補を徐々に絞っていく。不動点となるような type-var-to-type を求める。
    (while changed
      (setq changed nil)
      (dolist (type-var type-var-lst)
        (let ((previous-type (cdr (assoc type-var type-var-to-type)))
              (updated-type (pseudo-unify type-var type-var-to-type equations)))
          (unless (eq updated-type previous-type)
            (setq changed t)
            (push-cons-al (cons type-var updated-type) type-var-to-type))))
    )
type-var-to-type))



;;; 型変数と型変数-型連想リストと方程式をうけとり、その型変数の制約をまとめあげ、型を返す
;;; 型変数 α について処理を行う場合
;;; (α . integer-class) のようなものはスルー (α が integer-class の super であることがわかっても極論 object をとりうるだけかもしれないので)
;;; (α . β) も同様にスルー
;;; (number-class . α) は重要 (現在の α の型の候補を number-class の subclass であるように絞れる)
;;; (β . α) も同様にして現在の β の型の候補をもとに α の型の候補を絞れる
(defun pseudo-unify (type-var type-var-to-type separated-equations)
  (assert (every #'(lambda (one-eq) (eq (length one-eq) 1)) separated-equations) "separated equations should be an alist of (a . b)")
  (let ((right-class (cdr (assoc type-var type-var-to-type))))
    (dolist (equation separated-equations)
      (if (eq (cdr equation) type-var)
        (let ((left-class (if (variablep (car equation)) 
                                (cdr (assoc (car equation) type-var-to-type))    ; (β . α) パターン
                                (car equation))))    ; (number-class . α) パターン
          (setq right-class (specify-class left-class right-class)))))
  right-class))


;;; master で slave を縛る。 縛った上界のクラスを返す
;;; 集合的に捉えると、slave の要素のうち master 集合に含まれるもののみを抽出すればOK. 
;;; master, slave が共に通常クラス (含: nil-class)... (my-superclassp master slave) なら縛り追加なし、slave をそのまま返す。
;;;                                                   (my-superclassp slave master) なら master となる。
;;;                                                   どちらでもない場合は irrelevant. 型エラー?
;;; master のみ Union(a, b) ... (my-superclassp master slave) なら縛り追加なし、slave をそのまま返す。
;;;                             (my-superclassp slave master) なら masterとなる。(e.g. slave が object とか、 nil-class とか)
;;;                             (my-superclassp slave a) なら a, (my-superclassp slave b) なら b を返す。
;;;                             どれでもない場合は irrelevant.  型エラー?
;;; slave のみ Union(a, b) ... (my-superclassp master slave) なら縛り追加なし、slave をそのまま返す。(e.g. master が object とか、 nil-class とか)
;;;                            (my-superclassp slave master) なら master を返す。 (sequence型に対して integer-vectorという縛りが追加された時を考えよ)
;;;                            (my-superclassp master a) なら a, (my-superclassp master b) なら b を返す。
;;;                            どれでもない場合は irrelevant. 型エラー?
;;; 共に Union(a, b), Union(c, d) ... (my-superclassp master slave) なら縛り追加なし、slave をそのまま返す (master / slave 共に同じ Union の場合とか)
;;;                               ... (my-superclassp slave master) なら master となる。 (上記ケースで網羅されてしまうが)
;;;                               ... (my-superclassp slave a) なら a, (my-superclassp slave b) なら b を,
;;;                               ... (my-superclassp master c) なら c, (my-superclassp master d) なら d を返す。
;;;                               ... どれでもない場合は irrelevant. 型エラー?
(defun specify-class (master slave)
  (cond ((and (union-classp master) (union-classp slave))
            (let* ((master-component (union-to-component master))
                   (master-1 (elt master-component 0))    ; 要素数2と決め打ちするのは拡張性が低い?
                   (master-2 (elt master-component 1))
                   (slave-component (union-to-component slave))
                   (slave-1 (elt slave-component 0))
                   (slave-2 (elt slave-component 1)))
              (cond ((my-superclassp master slave) slave)
                    ((my-superclassp slave master) master)
                    ((my-superclassp slave master-1) master-1)    ; 今の所複数マッチはしないはず
                    ((my-superclassp slave master-2) master-2)
                    ((my-superclassp master slave-1) slave-1)
                    ((my-superclassp master slave-2) slave-2)
                    (t (error "cannot specify the class (master: union class, slave: union class)."))
              )))
        ((union-classp master)
            (let* ((master-component (union-to-component master))
                   (master-1 (elt master-component 0))
                   (master-2 (elt master-component 1)))
              (cond ((my-superclassp master slave) slave)
                    ((my-superclassp slave master) master)
                    ((my-superclassp slave master-1) master-1)
                    ((my-superclassp slave master-2) master-2)
                    (t (error "cannot specify the class (master: union class, slave: ordinary class)."))
              )))
        ((union-classp slave)
            (let* ((slave-component (union-to-component slave))
                   (slave-1 (elt slave-component 0))
                   (slave-2 (elt slave-component 1)))
              (cond ((my-superclassp master slave) slave)
                    ((my-superclassp slave master) master)
                    ((my-superclassp master slave-1) slave-1)
                    ((my-superclassp master slave-2) slave-2)
                    (t (error "cannot specify the class (master: ordinary class, slave: union class)."))
              )))
        (t
            (cond ((my-superclassp master slave) slave)
                  ((my-superclassp slave master) master)
                  (t (error "cannot specify the class (master and slave are ordinary classes)."))))
  ))



;;; types はクラスないし型変数、およびそのリスト
;;; 型変数の部分は型に置き換え、全ての型を文字列表記へと変換して返す (send cls :name) はシンボルを返すことに注意
(defun convert-to-ordinary-type (types type-var-to-type)
  (if (atom types)
    (if (classp types) 
      (string (send types :name))
      (string (send (cdr (assoc types type-var-to-type)) :name)))
    (let ((ans nil))
      (dolist (elm types)
        (cond ((and (consp elm) (not (eq (car elm) 'rest)))     ; (optional . integer) (repeat . α) (key . α) など
                  (if (classp (cdr elm))
                    (push (cons (car elm) (string (send (cdr elm) :name))) ans)
                    (push (cons (car elm) (string (send (cdr (assoc (cdr elm) type-var-to-type)) :name))) ans))
                   )
              ((consp elm) (push elm ans))    ; (rest)
              ((classp elm) (push (string (send elm :name)) ans))    ; #<metaclass #X55e4854af010 array> など
              (t (push (string (send (cdr (assoc elm type-var-to-type)) :name)) ans))    ; α など
        ))
      (reverse ans))))


