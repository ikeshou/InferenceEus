;;; inference_eusfunc.l からデータモードで読み込む
;;; (eval set-ftype-command-for-builtin-functions) を行えばマクロ実行される


;;; TYPE1 eusコアの関数、&key &optional &rest repeat ないもの
;;; TYPE2 repeat の追加
;;; TYPE3 &key &optional &rest の追加
;;; TYPE4 send 関連の追加


(in-package "INFERENCE-EUSFUNC")


(setq set-ftype-command-for-builtin-functions
  '(progn

;;; 述語
(set-ftype eq symbol object object)
(set-ftype eql symbol object object)
(set-ftype equal symbol object object)
(set-ftype superequal symbol object object)
(set-ftype null symbol object)
(set-ftype not symbol object)
(set-ftype atom symbol object)
(set-ftype every symbol sequence-class)
(set-ftype some symbol sequence-class)
(set-ftype functionp symbol object)
(set-ftype compiled-function-p symbol object)


;;; クラスとメソッド
(set-ftype classp symbol object)
(set-ftype subclassp symbol metaclass metaclass)
(set-ftype vector-class-p symbol object)
(set-ftype delete-method cons object)    ; なぜか成功時も失敗時も(7942 79)といったconsが返る
(set-ftype class-hierarchy cons metaclass)
(set-ftype system::list-all-classes cons nil-class)
;; system::find-method
(set-ftype system::method-cache cons (optional object))

;;; メッセージ送信
;; send
;; send-message
;; send-all

;;; インスタンス管理
;; instantiate
;; make-instance
(set-ftype copy-object object object)
(set-ftype become object object metaclass)
(set-ftype replace-object object object object)
(set-ftype class metaclass object)
(set-ftype derivedp symbol object metaclass)
(set-ftype slot object object metaclass object)

;;; 基本クラス
(set-ftype find-method cons object)    ; 成功時は(#<metaclass #X561a825ef9b8 object> (:slots #<compiled-code #X561a825b25d0>))のようなconsが、失敗時はnilが返る


;;; 比較演算
(set-ftype numerp symbol object)
(set-ftype integerp symbol object)
(set-ftype floatp symbol object)
(set-ftype zerop symbol number-class)
(set-ftype plusp symbol number-class)
(set-ftype oddp symbol integer-class)
(set-ftype evenp symbol integer-class)
(set-ftype /= symbol number-class number-class)
(set-ftype = symbol number-class number-class)
(set-ftype > symbol number-class number-class)
(set-ftype < symbol number-class number-class)
(set-ftype >= symbol number-class number-class)
(set-ftype <= symbol number-class number-class)

;;; 整数とビット毎の操作関数
(set-ftype mod number-class number-class number-class)
(set-ftype 1- integer-class integer-class)
(set-ftype 1+ integer-class integer-class)
(set-ftype logand integer-class (repeat integer-class))    ; 引数1つでもOK
(set-ftype logior integer-class (repeat integer-class))
(set-ftype logxor integer-class (repeat integer-class))
(set-ftype logeqv integer-class (repeat integer-class))
(set-ftype lognand integer-class (repeat integer-class))
(set-ftype lognor integer-class (repeat integer-class))
(set-ftype lognot integer-class integer-class)
(set-ftype logtest symbol integer-class integer-class)
(set-ftype logbitp symbol integer-class integer-class)
(set-ftype ash integer-class integer-class integer-class)
(set-ftype ldb integer-class integer-class integer-class integer-class)
(set-ftype dpb integer-class integer-class integer-class integer-class integer-class)

;;; 一般数値関数
(set-ftype + number-class (repeat number-class))    ; 引数1つでもOK
(set-ftype - number-class (repeat number-class))
(set-ftype * number-class (repeat number-class))
(set-ftype / number-class number-class number-class (repeat number-class))
(set-ftype abs number-class number-class)
(set-ftype round number-class number-class)
(set-ftype floor number-class number-class)
(set-ftype ceiling number-class number-class)
(set-ftype truncate number-class number-class)
(set-ftype float number-class number-class)
(set-ftype max number-class (repeat number-class))
(set-ftype min number-class (repeat number-class))
(set-ftype make-random-state integer-vector (optional integer-vector))
(set-ftype random number-class number-class (optional integer-vector))
;; reduce
(set-ftype rad2deg number-class number-class)
(set-ftype deg2rad number-class number-class)

;;; 基本関数
(set-ftype sin float-class number-class)
(set-ftype cos float-class number-class)
(set-ftype tan float-class number-class)
(set-ftype sinh float-class number-class)
(set-ftype cosh float-class number-class)
(set-ftype tanh float-class number-class)
(set-ftype asin float-class number-class)
(set-ftype acos float-class number-class)
(set-ftype atan float-class number-class)
(set-ftype asinh float-class number-class)
(set-ftype acosh float-class number-class)
(set-ftype atanh float-class number-class)
(set-ftype sqrt float-class number-class)
(set-ftype log float-class number-class)
(set-ftype exp float-class number-class)
(set-ftype expt number-class number-class number-class)


;;; シンボル
(set-ftype symbolp symbol object)
(set-ftype symbol-value object symbol)
(set-ftype symbol-function compiled-code symbol)
(set-ftype symbol-package package symbol)
(set-ftype symbol-name string symbol)
(set-ftype symbol-plist cons symbol)
(set-ftype boundp symbol symbol)
(set-ftype fboundp symbol symbol)
;; makeunboundなどない
(set-ftype get object symbol object)
(set-ftype putprop object symbol object object)
(set-ftype remprop object symbol object)
(set-ftype keywordp symbol object)
(set-ftype constantp symbol)
(set-ftype documentation string symbol (optional object))
(set-ftype gensym symbol (optional object))     ; 本当は optional Union(integer-class, string) だけど
(set-ftype gentemp symbol (optional string) (optional packagelike-class))

;;; パッケージ
(set-ftype find-symbol symbol string (optional packagelike-class))
(set-ftype make-symbol symbol string)
(set-ftype intern symbol string (optional packagelike-class) (optional symbol))    ; optional symbol の方は自信ない
(set-ftype list-all-packages cons nil-class)
(set-ftype find-package package object)
(set-ftype make-package package symbollike-class (nicknames object))    ; nicknames は Union(symbol, string , list of them)らしいけど 
(set-ftype in-package package symbollike-class (nicknames object))
(set-ftype package-name string object)
(set-ftype package-nicknames cons object)
(set-ftype rename-package package packagelike-class symbollike-class (optional object))
(set-ftype package-use-list cons object)
(set-ftype packagep symbol object)
(set-ftype use-package package packagelike-class (optional packagelike-class))
(set-ftype unuse-package package packagelike-class (optional packagelike-class))
(set-ftype shadow symbol symbol (optional packagelike-class))
(set-ftype export symbol symbol (optional packagelike-class))
(set-ftype unexport symbol symbol (optional packagelike-class))
(set-ftype import symbol symbol (optional packagelike-class))


;;; 一般列
(set-ftype elt object sequence-class integer-class)
(set-ftype length integer-class sequence-class)
(set-ftype subseq sequence-class integer-class (optional integer-class))
(set-ftype copy-seq sequence-class sequence-class)
(set-ftype reverse sequence-class sequence-class)
(set-ftype nreverse sequence-class sequence-class)
;; concatenate
(set-ftype coerce sequence-class sequence-class metaclass)    ; result-type に変更するが所詮sequence-class内での変更である
;; map
(set-ftype fill sequence-class sequence-class object (start integer-class) (end integer-class))
(set-ftype replace sequence-class sequence-class (start1 integer-class) (end1 integer-class) (start2 integer-class) (end2 integer-class))
(set-ftype sort sequence-class sequence-class compiled-code (optional compiled-code))
;; merge
(set-ftype merge-list cons cons cons compiled-code compiled-code)    ; こちらは key の関数 (absとか)が optional ではない
(set-ftype position integer-class object sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code) (count integer-class))
(set-ftype position-if integer-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code))
(set-ftype position-if-not integer-class compiled-code sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code) (count integer-class))
(set-ftype find object object sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype find-if object compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype find-if-not object compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype count integer-class object sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code))
(set-ftype count-if integer-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code))
(set-ftype count-if-not integer-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code))
(set-ftype remove sequence-class object sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code) (key compiled-code) (count integer-class))
(set-ftype remove-if sequence-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype remove-if-not sequence-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype remove-duplicates sequence-class sequence-class (start integer-class) (end integer-class) (key compiled-code) (test compiled-code) (test-not compiled-code) (key compiled-code) (count integer-class))
(set-ftype delete sequence-class object sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code) (key compiled-code) (count integer-class))
(set-ftype delete-if sequence-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype delete-if-not sequence-class compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype substitute sequence-class object object sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code) (count integer-class))
(set-ftype substitute-if sequence-class object compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype substitute-if-not sequence-class object compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype nsubstitute sequence-class object object sequence-class (start integer-class) (end integer-class) (test compiled-code) (test-not compiled-code) (key compiled-code) (count integer-class))
(set-ftype nsubstitute-if sequence-class object compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))
(set-ftype nsubstitute-if-not sequence-class object compiled-code sequence-class (start integer-class) (end integer-class) (key compiled-code) (count integer-class))

;;; リスト
(set-ftype listp symbol object)
(set-ftype consp symbol object)
(set-ftype car object cons)
(set-ftype cdr cons cons)
(set-ftype cadr object cons)
(set-ftype cddr cons cons)
(set-ftype caar object cons)
(set-ftype cdar cons cons)
(set-ftype caddr object cons)
(set-ftype caadr object cons)
(set-ftype cadar object cons)
(set-ftype caaar object cons)
(set-ftype cdadr cons cons)
(set-ftype cdaar cons cons)
(set-ftype cdddr cons cons)
(set-ftype cddar cons cons)
(set-ftype first object cons)
(set-ftype nth object number-class cons)
(set-ftype nthcdr cons number-class cons)
(set-ftype last cons cons)
(set-ftype butlast cons cons (optional integer-class))
(set-ftype cons cons object object)
(set-ftype list cons (repeat object))
;; list*
(set-ftype list-length integer-class cons)
(set-ftype make-list cons integer-class (initial-element object))
(set-ftype rplaca cons cons object)
(set-ftype rplacd cons cons object)
(set-ftype memq symbol object cons)
(set-ftype member object cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype assq cons object cons)
(set-ftype assoc object object cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype rassoc cons object cons)
(set-ftype pairlis cons cons cons (optional cons))
(set-ftype acons cons object object cons)
(set-ftype append cons (repeat cons))
(set-ftype nconc cons (repeat cons))
(set-ftype subst cons object object cons)
(set-ftype flatten cons cons)
(set-ftype adjoin cons object cons)
;; union
(set-ftype union cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype subsetp symbol cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype intersection cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype set-difference cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype set-exclusive-or cons cons cons (key compiled-code) (test compiled-code) (test-not compiled-code))
(set-ftype list-insert cons object integer-class cons)
(set-ftype mapc cons compiled-code (repeat cons))
(set-ftype mapcar cons compiled-code (repeat cons))
(set-ftype mapcan cons compiled-code (repeat cons))

;;; ベクトルと行列
(set-ftype vectorp symbol object)
(set-ftype vector vector (repeat object))
;; make-array (vector or array ができる厄介なやつ)
(set-ftype svref object vector integer-class)
(set-ftype aref object array (repeat integer-class))    ; 本当はvectorに対しても使えなくはないけどarray専用でしょう
(set-ftype vector-push array object array)
(set-ftype vector-push-extend array object array)
(set-ftype arrayp symbol object)
(set-ftype array-total-size integer-class array)
(set-ftype fill-pointer object array)
(set-ftype array-rank integer-class array)
(set-ftype array-dimensions integer-class array)
(set-ftype array-dimension integer-class array integer-class)
(set-ftype bit integer-class bit-vector integer-class)
;; bit-and
(set-ftype bit-and bit-vector bit-vector bit-vector (optional bit-vector))
(set-ftype bit-ior bit-vector bit-vector bit-vector (optional bit-vector))
(set-ftype bit-xor bit-vector bit-vector bit-vector (optional bit-vector))
(set-ftype bit-eqv bit-vector bit-vector bit-vector (optional bit-vector))
(set-ftype bit-nand bit-vector bit-vector bit-vector (optional bit-vector))
(set-ftype bit-nor bit-vector bit-vector bit-vector (optional bit-vector))
(set-ftype bit-not bit-vector bit-vector bit-vector (optional bit-vector))


;;; 文字と文字列
(set-ftype digit-charp symbol object)
(set-ftype alpha-charp symbol object)
(set-ftype upper-case-p symbol object)
(set-ftype lower-case-p symbol object)
(set-ftype alphanumeric-p symbol object)
(set-ftype char-upcase integer-class integer-class)
(set-ftype char-downcase integer-class integer-class)
(set-ftype char integer-class string integer-class)
(set-ftype schar integer-class string integer-class)
(set-ftype stringp symbol object)
(set-ftype string-upcase string string (start integer-class) (end integer-class))
(set-ftype string-downcase string string (start integer-class) (end integer-class))
(set-ftype nstring-upcase string string)
(set-ftype nstring-downcase string string)
(set-ftype string= symbol string string (start1 integer-class) (end1 integer-class) (start2 integer-class) (end2 integer-class))
(set-ftype string-equal symbol string string (start1 integer-class) (end1 integer-class) (start2 integer-class) (end2 integer-class))
(set-ftype string string object)
(set-ftype string< symbol string string)
(set-ftype string<= symbol string string)
(set-ftype string> symbol string string)
(set-ftype string>= symbol string string)
(set-ftype string-left-trim string string string)
(set-ftype string-right-trim string string string)
(set-ftype string-trim string string string)
(set-ftype substringp symbol string string)

;;; Foreign String
(set-ftype make-foreign-string foreign-string integer-class integer-class)    ; (unix:malloc int) returns int


;;; ハッシュテーブル
(set-ftype sxhash integer-class object)
(set-ftype make-hash-table hash-table (size integer-class) (test compiled-code) (rehash-size integer-class))
(set-ftype gethash object object hash-table)
(set-ftype remhash nil-class object hash-table)
(set-ftype maphash cons compiled-code hash-table)
(set-ftype hash-table-p symbol object)


;;; ストリーム
(set-ftype streamp symbol object)
(set-ftype input-stream-p symbol object)
(set-ftype output-stream-p symbol object)
(set-ftype io-stream-p symbol object)
(set-ftype open stream pathlike-class (direction symbol) (if-exists symbol) (if-does-not-exist symbol) (permission symbol) (buffer-size integer-class))
(set-ftype close symbol stream)
(set-ftype make-string-input-stream stream string)
(set-ftype make-string-output-stream stream integer-class)
(set-ftype get-output-stream-string string stream)
(set-ftype make-broadcast-stream stream (repeat stream))

;;; リーダ
;; read
;; read-delimited-list
(set-ftype read-line string (optional stream) (optional symbol) (optional symbol))
(set-ftype read-char integer-class (optional stream) (optional symbol) (optional symbol))
;; read-from-string
;; unread-char
;; peek-char
;; y-or-n-p
;; yes-or-no-p
(set-ftype readtable-p symbol object)
(set-ftype copy-readtable readtable (optional readtable) (optional readtable))
;; set-syntax-from-char
;; set-macro-character
;; get-macro-character
;; set-dispatch-macro-character
;; get-dispatch-macro-character

;;; プリンタ
;; print
;; ptin1
;; princ
;; terpri
;; finish-output
;; princ-to-string
;; prin1-to-string
;; format
;; pprint
;; print-functions
(set-ftype write-byte integer-class integer-class stream)
(set-ftype write-word integer-class integer-class stream)
(set-ftype write-long integer-class integer-class stream)
;; spaces
;; pf
;; pp-method
;; tpring
(set-ftype print-size integer-class object)


;;; プロセス間通信とネットワーク
;; map-file
;; make-msgq-input-stream
;; make-msgq-output-stream
;; make-socket-address
(set-ftype make-socket-port socket-port socket-address)
(set-ftype make-server-socket-stream socket-stream socket-port (optional integer-class))
(set-ftype make-client-socket-stream socket-stream socket-address (optional integer-class))
(set-ftype open-server symbol integer-class compiled-code)
(set-ftype connect-server stream string integer-class)

;;; 非同期入出力
(set-ftype select-stream cons cons integer-class)


;;; パス名
(set-ftype pathnamep symbol object)
(set-ftype pathname pathname string)
(set-ftype pathname-directory cons pathlike-class)
(set-ftype pathname-name string pathlike-class)
(set-ftype pathname-type string pathlike-class)
(set-ftype make-pathname pathname (host object) (device object) (directory object) (name object) (type object) (version object)) 
;; merge-pathnames
(set-ftype namestring string pathlike-class)
(set-ftype parse-namestring pathname pathlike-class)
(set-ftype true-name pathname pathlike-class)


;;; ファイルシステムインターフェース
(set-ftype probe-file symbol pathlike-class)
(set-ftype file-size integer-class pathlike-class)
(set-ftype directory-p symbol pathlike-class)
(set-ftype find-executable pathname pathlike-class)
(set-ftype file-write-date integer-class pathlike-class)
(set-ftype file-newer symbol pathlike-class pathlike-class)
(set-ftype object-file-p symbol pathlike-class)
(set-ftype directory cons (optional pathlike-class))
(set-ftype dir symbol (optional pathlike-class))    ; ls したときのような出力となるが、戻り値は symbol の模様


;;; 評価関数
(set-ftype identity object object)
(set-ftype eval object object)
;; apply
;; funcall
(set-ftype evalhook object compiled-code object)
(set-ftype eval-dynamic object symbol)
(set-ftype macroexpand cons object)
;; eval-when
;; proclaim
;; warn
;; error
;; install-error-handlerなどない


;;; 最上位レベルの対話
;; eustop
;; eussigなどない
(set-ftype sigint-handler symbol object object)
;; euserror
(set-ftype reset symbol nil-class)
;; exit
(set-ftype h nil-class nil-class)
;; !
(set-ftype new-history cons integer-class)


;;; コンパイル
;; euscomp
;; compile-file
(set-ftype compile cons symbol)
;; compile-file-if-src-newer


;;; ロード
;; load
;; load-files
(set-ftype provide symbol symbol)
;; require


;;; デバッグ補助
;; describe
;; describe-list
;; inspect
;; more
;; break
(set-ftype help nil-class symbol)
(set-ftype aporopos nil-class symbol)
(set-ftype aporopos-list cons symbol)
(set-ftype constants cons (optional string) (optional packagelike-class))
(set-ftype variables cons (optional string) (optional packagelike-class))
(set-ftype functions cons (optional string) (optional packagelike-class))
;; btrace
;; step-hook
;; step
;; trace
;; untrace
;; timing
(set-ftype time cons compiled-code)
;; sys:list-all-catchersなどない
;; sys:list-all-instances
;; sys-list-all-bindings
;; sys:list-all-special-bindings
;; dump-object
;; dump-structure
;; dump-loadable-structure
;; save
(set-ftype lisp-implementation-type string nil-class)
(set-ftype lisp-implementation-version string nil-class)


  )    ; for progn
)    ; for setq


(in-package :USER)
