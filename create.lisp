(defpackage #:create
  (:use #:c2cl #:c2mop)
  (:export #:create-keyword-expand
           #:create-first-arguments
           #:create-keyword-arguments
           #:create
           #:create-list))
(in-package #:create)

(defgeneric create-keyword-expand (object keyword entries))

(defmethod create-keyword-expand (object keyword entries)
  (declare (ignore object keyword))
  `',entries)

(defgeneric create-first-arguments (object list))

(defmethod create-first-arguments (object list)
  (declare (ignore object list)))


(defgeneric create-keyword-arguments (object list))

(defmethod create-keyword-arguments (object list)
  (declare (ignore object))
  list)

(labels ((expand-lists (object lists)
           (when lists
             (destructuring-bind ((keyword . body) . lists) lists
               `(,keyword ,(create-keyword-expand object keyword body) ,@(expand-lists object lists))))))
  (defmacro create (name &rest rest &aux (object (let ((class (find-class name)))
                                                            (finalize-inheritance class)
                                                            (class-prototype class))))
    (let ((first (create-first-arguments object rest))
          (keyword (create-keyword-arguments object rest)))
      `(make-instance ',name ,@first ,@(expand-lists object keyword)))))

(defmacro create-list (class &body body)
  `(list ,@(mapcar (lambda  (arg) `(create ,class ,@arg)) body)))
