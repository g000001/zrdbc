;;;; zrdbc.lisp -*- Mode: Lisp;-*- 
(cl:in-package "https://github.com/g000001/zrdbc#internals")


(in-syntax zrdbc-syntax)


(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *block-name* '#:top-level)

(define-method-combination dbc ()
  ((in* (:in))
   (out* (:out))
   (pri* () :required T))
  (let ((results (gensym "results-"))
        (gf-name (and pri* (generic-function-name (method-generic-function (car pri*))))))
    `(progn
       (and ,(not (null in*))
            (or ,@(loop :for in :in (reverse in*) :collect `(call-method ,in))
                (error "Precondition error.~%Client: ~S~%Supplier: ~S"
                       ',gf-name
                       *block-name*)))
       (let ((,results (multiple-value-list (call-method ,(car pri*) ,(cdr pri*)))))
         (declare (dynamic-extent ,results))
         (or (and ,@(loop :for out :in (reverse out*) :collect `(apply (call-method ,out) ,results)))
             (error "Postcondition error.~%Supplier: ~S~%Client: ~S" 
                    ',gf-name
                    *block-name*))
         (values-list ,results)))))


(define-method-combination invariant ()
  ((pri *))
  `(and ,@(loop :for m :in (reverse pri) :collect `(call-method ,m)))))


(defclass dbc-class (cl:standard-class)
  ()
  (:metaclass cl:standard-class))


(defmethod validate-superclass ((c dbc-class) (s cl:standard-class))
  T)


(defclass dbc-object (cl:standard-object)
  ()
  (:metaclass dbc-class))


#|(defmethod initialize-instance :around ((class dbc-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (some (lambda (class) (subtypep class <dbc-object>))
            direct-superclasses)
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses
             (append direct-superclasses (list <dbc-object>))
             initargs)))|#
#||

Error: Error during finalization of class #<dbc-class dbc-object 427D3A2B23>: Cannot compute class precedence list for class: #<dbc-class dbc-object 427D3A2B23>
||#


#|(defmethod reinitialize-instance :around ((class dbc-class) &rest initargs 
                                             &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (unless direct-superclasses-p 
    (return-from reinitialize-instance (call-next-method)))
  (if (some (lambda (class) (subtypep class <dbc-object>))
            direct-superclasses)
      (break "~S" (call-next-method))
      (break "~S" (apply #'call-next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (list <dbc-object>))
             initargs))))|#


(defgeneric invariant (x)
  (:method-combination invariant))


(defmethod slot-value-using-class ((class dbc-class)
                                   instance
                                   slot)
  (invariant instance)
  (multiple-value-prog1 (call-next-method)
    (invariant instance)))


(defmethod (setf slot-value-using-class) 
           (value (class dbc-class) instance slot)
  (invariant instance)
  (multiple-value-prog1 (call-next-method)
    (invariant instance)))


(defmethod shared-initialize :after ((obj dbc-object) slots &key)
  (invariant obj))


(defun get-block-name (env)
  #+sbcl (caar (sb-c::lexenv-blocks env))
  #+lispworks (caar (compiler::compiler-environment-benv env)))


(defmacro def (name (&rest args) 
                    (&rest values)
                    (&body in)
                    (&body out)
                    &body main)
  (check-type (car in) (eql :in))
  (check-type (car values) (eql values))
  (check-type (car out) (eql :out))
  (let ((in (cdr in))
        (out (cdr out))
        (result-vars (cdr values)))
    `(progn
       (defgeneric ,name (,@args)
         (:method-combination dbc))
       ,(and in `(defmethod ,name :in (,@args) ,@in))
       (defmethod ,name (,@args) ,@main)
       ,(and out `(defmethod ,name :out (,@args) (lambda (,@result-vars) ,@out)))
       (define-compiler-macro ,name (&rest args &environment env)
         `(let ((*block-name* ',(get-block-name env)))
            (funcall #',',name ,@args))))))


;;; *EOF*
