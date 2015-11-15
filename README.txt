This system exports the two macros for making instances in an user-friendly way:

  create <class> &rest <args>
    arguments:
      class:
        the class, not evaluated
      args:
        arguments to make-instance expandet in an user-defined way
    returns:
      a new instance of class <class>
    example:
      ;;;default behaviour
      (create class (:a 1) (:b 2 3))
      ;;expands to
      (make-instance 'class :a '(1) :b '(2 3))

  create-list <class> &body <body>
    arguments:
      class:
        the class, not evaluated
      args:
        a list of args
    returns:
      a list of new instances of class <class>
    example:
      (create-list class (:a 1) (:b 2 :c 3))
      ;;expands to
      (list (create class :a 1) (create class :b 2 :c 3))


You can define the behaviour of expanding the arguments to create by defining these three generic functions:

  create-keyword-expand <object> <keyword> <entries>
    example:
      ;;;define a keyword to be the body of a funciton
      (defclass function-class () ;the new class
        ((function :initarg :function :type function)))
      ;;define the method
      (defmethod create-keyword-expand ((object function-class) (keyword (eql :function)) entries)
        `(lambda () ,@entries))
      (create function-class (:function (body1) (body2)))
      ;; will expand to (make-instance 'function-class :function (lambda () (body1) (body2)))
    
  create-first-arguments <object> <list>
    the list returned by this will be passed to create without calling create-keyword-expand
  create-keyword-arguments <object> <list>
    the list returned by this will be passed to create after calling create-keyword-expand to each
    example:
      ;;;name some class without using a keyword
      (defmethod create-first-arguments ((object some-class) list)
        `(:name ,(car list)))
      (defmethod create-first-arguments ((object some-class) list)
        ,(cdr list))
      (create some-class "Name" (:arg1 …) …)
      ;;will expand to (make-instance 'some-class :name "Name" :arg1 …)
      



