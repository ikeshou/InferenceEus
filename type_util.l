#|
型推論の前のソースファイルの前処理を行う際に必要になる関数が定義されたモジュール
list や alist への破壊的な追加を行うマクロもここで定義されている
|#

;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")


(require :FIX "fixed_hash.l")


;;; dot pair を alist 先頭へ破壊的に追加する (但し nil を push した場合変化しない)
;;; 第一引数がそのまま先頭に入る
;;; (equation (alist) が１つも立たなかった時、それらを追加した equations (alist) の表記は nil であるべき)
;;; (let ((x '(a . b))
;;;       (y '((c . d))))
;;;   (push-cons-al x y))
;;; => ((a . b) (c . d))
(defmacro push-cons-al (from-cons to-alist)
  `(unless (null ,from-cons) (setf ,to-alist (cons ,from-cons ,to-alist))))

;;; alist を alist 先頭へ破壊的に追加する (但し nil を push した場合変化しない)
;;; 第一引数の各要素がその順で先頭に入る
;;; (equations (alist) が nil の時、それらをまとめた equations (alist) の表記は nil であるべき)
;;; (let ((x '((a . b) (c . d))
;;;       (y '((e . f))))
;;;   (push-al-al x y))
;;; => ((a . b) (c . d) (e . f))
(defmacro push-al-al (from-alist to-alist)
  `(setf ,to-alist (append ,from-alist ,to-alist)))


;;; cons を cons 先頭へ破壊的に追加する
;;; 第一引数の各要素がその順で先頭に入る。push-al-al と同じ
;; (let ((x '(a b))
;;       (y '(c d)))
;;   (push-cons-cons x y))
;; => (a b c d)
(defmacro push-cons-cons (from-alist to-alist)
  `(setf ,to-alist (append ,from-alist ,to-alist)))


;;; 要素を list 末尾へ破壊的に追加する (組み込みマクロ push, pop は先頭に対する操作)
;;; (let ((p (list 1 2)))
;;;    (push-end 3 p))
;;; => (1 2 3)
(defmacro push-end (i lst)
  `(setf ,lst (append ,lst (list ,i))))



(defun source-to-sexp-vec (fname)
  "ソースファイルの S 式を vector に配置して返す"
  (let ((instream nil)
        (sexp nil)
        (sexp-buf nil))
    (with-open-file (instream fname :direction :input)
      (while (not (eq (setq sexp (read instream nil 'eof)) 'eof))
        ;; 意味のない直書きの atom, nil をはじいておく。vector の要素は cons と保障
        (when (consp sexp)
          (push sexp sexp-buf)))    ; 先頭に追加される
      (apply #'vector (reverse sexp-buf)))))


(defun collect-user-vars (sexp-vec)
  "
  vector に配置された S 式の集まりからユーザー定義変数を探し、辞書に登録して返す
  (defvar ...), (defparameter ...), (defconstant ...), (setq ...), (setf sym ...) を探索
  (後者ふたつは大域領域で宣言されると大域変数として振る舞うため)
  (関数内部で大域変数を生成するようなコードは今は考えない)
  "
  (let ((var-set (fix:make-hash-table)))
    (dotimes (i (length sexp-vec))
      (let ((sexp (aref sexp-vec i)))
        (when (or (member (car sexp) '(defvar defparameter defconstant setq))
                  (and (eq (car sexp) 'setf) (symbolp (cadr sexp))))
          (fix:sethash (cadr sexp) var-set i))))
    var-set))


(defun collect-user-funcs (sexp-vec)
  "
  vector に配置された S 式の集まりからユーザー定義関数を探し、辞書に登録して返す
  (defun ...) を探索
  "
  (let ((func-set (fix:make-hash-table)))
    (dotimes (i (length sexp-vec))
      (let ((sexp (aref sexp-vec i)))
        (when (eq (car sexp) 'defun)
          (let ((fn (car (cdr sexp))))
            (fix:sethash fn func-set i)))))
    func-set))
  


(defun collect-refs (sexps user-vars user-funcs local-vars local-funcs &optional (func-flag nil))
  "
  make-adj の補助関数
  sexps は atom or cons
  user-vars, user-funcs は大域変数、大域関数の辞書
  local-vars, local-funcs は局所変数、局所関数のリスト (新しいほど先頭)
  出現した大域関数および変数に対応する番号を集め、リストにして返す

  (let ...), (let* ...), (flet ...), (labels ...)
  これらによる局所変数、関数の導入を管理し、大域判定を行う必要がある (名前が衝突し局所変数を参照している可能性がある）

  Note:
    (高階関数 'fn-sym arg) パターンに未対応。これも関数領域から探さないと
    #'(lambda (arg) body) パターンに未対応。
  "
  (let ((refs nil))
    (labels
      ;; user-vars, user-funcs を引数なしに参照し、refs にうまくシンボルをためこむために内部関数で実装
      ;; シンボルのリストを返すのではなく担当範囲 S 式の該当シンボルを破壊的に refs にためこむ
      ((collect-1 (sexps local-vars local-funcs &optional (func-flag nil))
        (if (consp sexps)
            ; cons
            (let ((head (car sexps)))
              (cond
                ; quote はスキップしないとだめ (評価されないので出現シンボルは変数ではない)
                ((eq head 'quote)
                  nil)
                ; (function ...) 
                ((and (eq head 'function) (= (length sexps) 2))
                  (collect-1 (cadr sexps) local-vars local-funcs t))  ; func-flag をたてる                
                ; let, let*
                ((member head '(let let*))
                    (let ((bind (cadr sexps))
                          (body (caddr sexps))
                          (new-local-vars local-vars))
                      ;; 束縛部分の処理、デフォルト値指定部も調査する
                      (dolist (elm bind)
                        (if (consp elm)
                          ; 初期値あり
                          (progn
                            (assert (symbolp (car elm)))  ; elm は (var sexp)
                            (collect-1
                                (cadr elm)
                                (if (eq head 'let) local-vars new-local-vars)
                                local-funcs)
                            (push (car elm) new-local-vars))
                          ; 初期値なし
                          (progn
                            (assert (symbolp elm))  ; elm は var
                            (push elm new-local-vars))))
                      ;; 本体の処理
                      (collect-1 body new-local-vars local-funcs)))
                ; flet, labels
                ((member head '(flet labels))
                    (let ((bind (cadr sexps))
                          (body (caddr sexps))
                          (new-local-funcs local-funcs))
                      ;; 束縛部分の処理
                      (dolist (elm bind)
                        (assert (and (consp elm) (symbolp (car elm)) (= (length elm) 3)))  ; elm は (fn (args) sexp)
                        (collect-1
                            (caddr elm)
                            (append (collect-argvars (cadr elm)) local-vars)  ; 仮引数を加える
                            (if (eq head 'flet) local-funcs new-local-funcs))
                        (push (car elm) new-local-funcs))
                      ;; 本体の処理
                      (collect-1 body local-vars new-local-funcs)))
                ; それ以外
                (t 
                  (progn
                    (collect-1 (car sexps) local-vars local-funcs t)  ; 先頭は func-flag をたてる
                    (dolist (elm (cdr sexps))
                      (collect-1 elm local-vars local-funcs))))))
            ; atom (includes nil). 再帰の基底ケースの処理
            ; refs に参照していたやつらのノード番号を追加。
            ; ある関数ボディや変数定義ボディで複数回同じ関数や変数を参照すると重複が生まれる。
            ; そのままだと辺が複数本はられてしまうので最後に取り除く。
            (when (and (symbolp sexps)
                       (or (and func-flag (not (member sexps local-funcs))) ; 関数かつローカル関数じしょにはない
                           (and (not func-flag) (not (member sexps local-vars)))))  ; 変数かつローカル変数辞書にはない
              (if func-flag
                (when (fix:gethash sexps user-funcs) ; ユーザ関数でない場合は何もしない
                  (push (fix:gethash sexps user-funcs) refs))
                (when (fix:gethash sexps user-vars)  ; ユーザ変数でない場合は何もしない
                  (push (fix:gethash sexps user-vars) refs)))))
        ))
      ;; 実質本体である内部関数 collect-1 を実行
      (collect-1 sexps local-vars local-funcs)
      (remove-duplicates refs))))



(defun collect-argvars (arg-section)
  "
  make-adj, collect-refs の補助関数
  関数定義の (a b &optional c d &rest ls &key f g) なる引数指定部分リストを受け取り、
  新たに定義されたシンボルの羅列を逆に並べたもの (g f ls d c b a) を返す
  "
  (let ((argvars nil))
    (dolist (elm arg-section)
      (if (consp elm)
        (progn
          (assert (atom (car elm)))
          (push (car elm) argvars))
        (unless (= (char (string elm) 0) (char "&" 0))
          (push elm argvars))))
    argvars))



(defun make-adj (sexp-vec user-vars user-funcs)
  "
  convert-to-dag の補助関数
  各 S 式の index をノード番号と対応づける。
  変数定義、関数定義の内部での参照関係を被参照 -> 参照なる有向辺と捉えた隣接リストを生成する
  "
  (let* ((size (length sexp-vec))
         (adj (instantiate vector size)))
    (flet ((add-edges (lst v)
              ;; lst の各番号のノード (callee) -> u (caller) へ辺を張るよう adj へ追記
              ;; 挙動を追いやすくするために一応 push-end にしている
              (dolist (u lst)
                (push-end v (aref adj u)))))
      (dotimes (i size)
        (let* ((sexp 
                (aref sexp-vec i))
               (refs 
                 (cond
                   ((or (member (car sexp) '(defvar defparameter defconstant setq))
                        (and (eq (car sexp) 'setf) (symbolp (cadr sexp))))
                     (collect-refs 
                        (caddr sexp)
                        user-vars
                        user-funcs
                        nil
                        nil))
                   ((eq (car sexp) 'defun)
                     (collect-refs 
                        `(progn ,@(cdddr sexp)) ; 暗黙の progn
                        user-vars
                        user-funcs
                        (collect-argvars (caddr sexp))  ; 仮引数による名前の束縛
                        (list (cadr sexp)))))))  ; 関数自身による名前の束縛（グラフ構築時に再帰関数の場合ループができるのを防ぐ）
          (add-edges refs i))))
      adj))



(defun topological-bfs (adj)
  "
  convert-to-dag の補助関数
  隣接リスト (list の vector) をもとにトポロジカルソートを行い、ソート結果のノード番号の integer-vector を返す
  DAG でなかった場合 RuntimeError
  "
  (let* ((size (length adj))
         (dimensions (instantiate integer-vector size))
         (queue nil)
         (sorted nil))    ; 先頭に push していき最後に反転
    ;; 入次数計算
    (dotimes (i size)
      (dolist (v (aref adj i))
        (incf (aref dimensions v))))
    ;; スタート時の queue を作成
    (dotimes (i size)
      (when (= (aref dimensions i) 0)
        (push-end i queue)))
    ;; BFS
    (while (not (null queue))
      (let ((top (pop queue)))
        (assert (= (aref dimensions top) 0))
        (push top sorted)
        (dolist (v (aref adj top))
          (decf (aref dimensions v))
          (when (= (aref dimensions v) 0)
            (push-end v queue)))))
    ;; 全てのノードが次数 0 になり queue へ追加されきっていたら DAG であった
    (if (= (length sorted) size)
      (apply #'integer-vector (reverse sorted))
      (error "souce file cannot be converted to DAG. (probably circular refs?)"))))


(defun arrange-sexp (sexp-vec mapping)
  "
  convert-to-dag の補助関数
  トポロジカルソート結果をもとに再配置した vector を生成して返す
  "
  (let* ((size (length sexp-vec))
         (arranged (instantiate vector size)))
    (dotimes (i size)
      (setf (aref arranged i)   ; arranged[i] には
            (aref sexp-vec (aref mapping i))))  ; 計算したノード番号に対応する S 式が入る
    arranged))


(defun convert-to-dag (sexp-vec user-vars user-funcs)
  "
  ワンパスで型推論できるよう、 vector に配置された S 式を再配置した vector を新たに生成して返す
  変数定義、関数定義の内部での参照関係を有向辺だと考え有向グラフを構築
  トポロジカルソートにより DAG に変換
  "
  (arrange-sexp sexp-vec (topological-bfs (make-adj sexp-vec user-vars user-funcs))))




;; ; userful for debug
(defun convert-all (fname)
  (let* ((vec (source-to-sexp-vec fname))
         (u-v (collect-user-vars vec))
         (u-f (collect-user-funcs vec))
         (adj (make-adj vec u-v u-f))
         (topo (topological-bfs adj))
         (arranged (convert-to-dag vec u-v u-f)))
    (format t "adj: ~a~%" adj)
    (format t "topo: ~a~%" topo)
    (format t "arranged: ~a~%" arranged)
    arranged))