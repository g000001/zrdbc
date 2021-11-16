;;;; readtable.lisp -*- Mode: Lisp;-*- 
(cl:in-package "https://github.com/g000001/zrdbc#internals")


(defmacro in-syntax (readtable)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,readtable)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((rt (copy-readtable nil)))
    #|(set-macro-character )|#
    #|(set-dispatch-macro-character )|#
    (defconstant zrdbc-syntax rt)))


;;; *EOF*


