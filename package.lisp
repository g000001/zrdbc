;;;; package.lisp -*- Mode: Lisp;-*- 
(cl:in-package "COMMON-LISP-USER")


(defpackage "https://github.com/g000001/zrdbc"
  (:nicknames zrdbc)
  (:use)
  (:export dbc dbc-class dbc-object invariant def)
  ;; syntax
  (:export zrdbc-syntax))


(defpackage "https://github.com/g000001/zrdbc#internals"
  (:use "https://github.com/g000001/zrdbc"
        "C2CL"
        "FIVEAM"))


;;; *EOF*
