;;;; zrdbc.lisp -*- Mode: Lisp;-*- 
(cl:in-package "https://github.com/g000001/zrdbc#internals")


(in-syntax zrdbc-syntax)


(eval-when (:compile-toplevel :load-toplevel :execute)

(define-method-combination dbc ()
  ((in* (:in))
   (out* (:out))
   (pri* () :required T))
  (let ((results (gensym "results-")))
    `(progn
       (and ',in*
            (or ,@(loop :for in :in (reverse in*) :collect `(call-method ,in))
                (error "Precondition error")))
       (let ((,results (multiple-value-list (call-method ,(car pri*) ,(cdr pri*)))))
         (declare (dynamic-extent ,results))
         (or (and ,@(loop :for out :in (reverse out*) :collect `(apply (call-method ,out) ,results)))
             (error "Postcondition error"))
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


;;; *EOF*
