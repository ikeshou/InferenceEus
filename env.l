#|
型環境を表現するためのモジュール
そもそもスコープチェーンを使わないと（仮引数で辞書受け渡し時に参照でわたすので）管理が大変
さらに言うと setq 由来の型情報を追跡するためにスコープチェーンが結局必要となる
|#

;; (provide :type1)
;; (unless (find-package "TYPE1") (make-package "TYPE1"))
;; (in-package "TYPE1")


(require "type_util.l")
(require :fix "fixed_hash.l")


;;; ht スロットに環境辞書 fix:hashtable を、outer スロットに親環境 env をもつ
(defclass env :super object :slots (ht outer))

(defun make-env (&key (outer nil) (size 10) (test #'eq) (rehash-size 1.7) (hash #'sxhash) (not-found nil))
  (make-instance env :ht
                      (instance fix:hash-table :init :size size
                                               :test test
                                               :rehash-size rehash-size
	                                             :hash hash	
		                                           :not-found not-found)
                     :outer outer))


;;; 基本的に操作関数は中のハッシュテーブルに作用する
;;; getenv, setenv はスコープチェーンをたどる
;;; スコープチェーンによる実装において、各スコープは担当範囲で新たに定義された変数についてのみ情報を管理するのがポイントである
(defun getenv (key ev)
  (let ((ret (fix:gethash key (env-ht ev)))
        (outer (env-outer ev)))
    (if ret 
      ret
      (if outer (getenv key outer)))))

(defun setenv (key ev val)
  (let ((ret (fix:gethash key (env-ht ev)))
        (outer (env-outer ev)))
    (if (or ret (null outer))
      (fix:sethash key (env-ht ev) val)   ; キーが見つかった or 見つからないが一番上まで上りきった場合そこで登録
      (setenv key outer val))))

;;; スコープチェーンをたどらずにバリューを得たい場合
(defun getenv-here (key ev)
  (fix:gethash key (env-ht ev)))
;;; スコープチェーンをたどらずにキーバリューを設定したい場合
(defun setenv-here (key ev val)
  (fix:sethash key (env-ht ev) val))
;;; そもそも削除を使うことはない。あったとしてもスコープチェーンをたどることは無いはずなので remenv-here とする
(defun remenv-here (key ev)
  (fix:remhash key (env-ht ev)))
;;; その他
(defun env-p (x) (derivedp x env))
(defun mapenv (func ev) (fix:maphash func (env-ht ev)))
(defun clrenv (ev) (fix:clrhash (env-ht ev)))
