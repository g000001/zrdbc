;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package "https://github.com/g000001/zrdbc#internals")


(in-syntax zrdbc-syntax)


(def-suite* zrdbc)


(defgeneric integer->integer->integer (x y)
  (:method-combination dbc))


(defmethod integer->integer->integer :in ((x number) (y number))
  (and (integerp x)
       (integerp y)
       (not (zerop y))))


(defmethod integer->integer->integer ((x number) (y number))
  (/ x y))


(defmethod integer->integer->integer :out ((x integer) (y integer))
  #'integerp)


#||
(mc-expand #'integer->integer->integer 'dbc nil 1 2)
→
(progn
  (or (call-method
       #<standard-method integer->integer->integer (:in) (number
                                                          number) 414009E67B>)
      (error "in"))
  (let ((#:|results-166692|
         (multiple-value-list (call-method
                               #<standard-method integer->integer->integer nil (number
                                                                                number) 414009E45B>
                               nil))))
    (declare (dynamic-extent #:|results-166692|))
    (or (and (apply (call-method
                     #<standard-method integer->integer->integer (:out) (integer
                                                                         integer) 40202D35A3>)
                    #:|results-166692|))
        (error "out"))
    (values-list #:|results-166692|)))
||#

(defmacro dbc-run (form)
  `(multiple-value-bind (ans error)
                        (ignore-errors ,form)
     `(,',form :result ,ans :error 
               ,(and error
                     (format nil 
                             (simple-condition-format-control error)
                             nil
                             (simple-condition-format-arguments error))))))


(test |test 0|
  (is (equal (list 
              (dbc-run (integer->integer->integer 1 8))
              (dbc-run (integer->integer->integer 1 0))
              (dbc-run (integer->integer->integer 10 2))
              (dbc-run (integer->integer->integer 10 1/2)))
             '(((integer->integer->integer 1 8) :result nil :error "out")
               ((integer->integer->integer 1 0) :result nil :error "in")
               ((integer->integer->integer 10 2) :result 5 :error nil)
               ((integer->integer->integer 10 1/2) :result nil :error "in")))))


(defclass date (dbc-object) 
  ((year :initarg :year :accessor year)
   (month :initarg :month :accessor month)
   (day  :initarg :day :accessor day))
  (:metaclass dbc-class)
  (:default-initargs :year 1900 :month 1 :day 1))


(defgeneric invariant (x)
  (:method-combination invariant))


(defmethod invariant year ((date date))
  (etypecase (year date)
    (integer T)))


(defmethod invariant month ((date date))
  (etypecase (month date)
    ((integer 1 12) T)))


(defun leap-year-p (n)
  #+lispworks (sys::leap-year-p n))


(defmethod invariant day ((date date))
  (etypecase (day date)
    ((eql 29) (or (/= 2 (month date))
                  (leap-year-p (year date))))
    ((eql 30) (/= 2 (month date)))
    ((eql 31) (typep (month date) '(member 1 3 5 7 8 10 12)))))


(test |test 1|
  (signals error (make-instance 'date :year 100 :month 2 :day -2)))


(defclass date2 ()
  ((year :initform 1900 :initarg :year :accessor year)
   (month :initform 1 :initarg :month :accessor month)
   (day :initform 1 :initarg :day :accessor day))
  (:metaclass dbc-class))


#||
(change-class (make-instance 'date2 :year 100 :month 2 :day -2)
              'date)
>>> error
||#

(defclass datetime (date)
  ((hour :initarg :hour :accessor hour)
   (minute :initarg :minute :accessor minute)
   (second  :initarg :second :accessor sec))
  (:metaclass dbc-class)
  (:default-initargs :hour 0 :minute 0 :second 0))


(defmethod invariant hour ((dt datetime))
  (etypecase (hour dt)
    ((integer 0 24) T)))


(defmethod invariant minute ((dt datetime))
  (etypecase (minute dt)
    ((integer 0 59) T)))


(defmethod invariant second ((dt datetime))
  (etypecase (sec dt)
    ((integer 0 59) T)))


;;; *EOF*
