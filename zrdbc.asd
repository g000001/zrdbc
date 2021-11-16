;;;; zrdbc.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :zrdbc
  :serial t
  :depends-on (:fiveam
               :closer-mop)
  :components ((:file "package")
               (:file "readtable")
               (:file "zrdbc")
               #|(:file "test")|#))


(defmethod perform ((o test-op) (c (eql (find-system :zrdbc))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/zrdbc#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'zrdbc)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
