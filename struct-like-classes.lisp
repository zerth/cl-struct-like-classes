;;;; helper macros for defining classes having struct-like definitions.

(defpackage :net.mwatters.struct-like-classes
  (:nicknames :struct-like-classes)
  (:use :common-lisp :iterate)
  (:import-from :alexandria
   :with-gensyms
   :symbolicate)
  (:export
   :define-struct-like-class
   :define-struct-like-bound-class))


(in-package :net.mwatters.struct-like-classes)


(define-method-combination %appending% (&optional (order :most-specific-first))
  ((methods (%appending%) :order order))
  "a method combination which appends the results of all methods in
the specified order, defaulting to most-specific-first."
  `(append ,@(mapcar (lambda (m)
                       `(call-method ,m))
                     methods)))


(defgeneric slot-args-for-copy (obj)
  (:method-combination %appending%)
  (:method %appending% ((obj t)) nil)
  (:documentation "return a list the initargs and values necessary to
create a new instance of OBJ having the same slot values."))


(defmacro define-struct-like-class (name-and-opts supers &body slots)
  "define a class which can be defined like a struct and which has
similar helper methods, to aid in development when definitions might
need changing and to save some typing."
  ;; fixme; option to replace w/defstruct forms when deploying? (but
  ;; method definition semantics might be different; consult
  ;; hyperspec)
  (let* ((class-name (if (atom name-and-opts)
                         name-and-opts
                       (car name-and-opts)))
         (opts (if (atom name-and-opts)
                   nil
                 (cdr name-and-opts)))
         ;; note: for now, only processed option is :constructor, and
         ;; only processed value is nil or a symbol name.
         (constructor-opt (iter
                            (for spec in opts)
                            (when (atom spec)
                              (error "unknown option: ~S" spec))
                            (destructuring-bind (opt-name &rest opt-params) spec
                              (case opt-name
                                (:constructor
                                 (unless (or (not opt-params)
                                             (= 1 (length opt-params)))
                                   (error "unhandled option: ~S" spec))
                                 (return (car opt-params)))
                                (t
                                 (error "unknown option: ~S" spec))))
                            (finally (return t)))))
    (with-gensyms (obj args)
      (let* ((slot-names (list))
             (conc-name (when constructor-opt
                          (if (eq t constructor-opt)
                              (symbolicate 'make- class-name)
                            constructor-opt)))
             (copier-name (symbolicate 'copy- class-name))
             (pred-name (symbolicate class-name '-p)))
        `(progn

           (defclass ,class-name ,supers
             (,@(iter
                  (for slot-spec in slots)
                  (for slot-name = (if (atom slot-spec)
                                       slot-spec
                                     (car slot-spec)))
                  (for slot-initform = (if (atom slot-spec)
                                           nil
                                         (cadr slot-spec)))
                  (for slot-opts = (if (atom slot-spec)
                                       nil
                                     (cddr slot-spec)))
                  (for slot-initarg = (intern (string (symbol-name slot-name))
                                              (find-package :keyword)))
                  (for accessor-name = (symbolicate class-name '- slot-name))
                  (collect
                   `(,slot-name :initform ,slot-initform
                                :initarg ,slot-initarg
                                :accessor ,accessor-name
                                ;; assume compatible w/defclass for now:
                                ,@slot-opts))
                  (push slot-name slot-names))))

           (defgeneric ,pred-name (,obj)
             (:method ((,obj t)) nil)
             (:method ((,obj ,class-name)) t))

           (defmethod slot-args-for-copy %appending% ((,obj ,class-name))
             (list ,@(iter
                       (for slot-spec in slots)
                       (for slot-name = (if (atom slot-spec)
                                            slot-spec
                                          (car slot-spec)))
                       (for slot-initarg = (intern (string (symbol-name slot-name))
                                                   (find-package :keyword)))
                       (for accessor-name = (symbolicate class-name '- slot-name))
                       (appending `(,slot-initarg (,accessor-name ,obj))))))

           ,@(when constructor-opt

               `((defun ,copier-name (,obj)
                   (apply #'make-instance (class-of ,obj)
                          (slot-args-for-copy ,obj)))

                 (defun ,conc-name (&rest ,args &key ,@slot-names
                                          &allow-other-keys)
                   (declare (ignore ,@slot-names))
                   (apply #'make-instance ',class-name ,args)))))))))



(defmacro define-struct-like-bound-class (name-and-opts supers (bound-symbol
                                                                &optional binding-form)
                                                        &body slots)
  "define a struct-like class which may be bound to the given
BOUND-SYMBOL.  if BINDING-FORM is supplied and is not T, a symbol
macro is defined which expands to that form.  if it is T, no var or
symbol macros are defined. otherwise, a variable and a WITH- macro are
defined for the variable named by that symbol."
  (with-gensyms (v forms)
    (let* ((class-name (if (atom name-and-opts)
                           name-and-opts
                         (car name-and-opts)))
           (bound-accessors (iter
                              (for slot-spec in slots)
                              (for slot-name = (if (atom slot-spec)
                                                   slot-spec
                                                 (car slot-spec)))
                              (for accessor-name = (symbolicate class-name '- slot-name))
                              (collect (cons (symbolicate accessor-name '*) accessor-name)))))
      `(progn
         (define-struct-like-class ,name-and-opts ,supers ,@slots)
         ,@(if binding-form
               (if (eq t binding-form)
                   nil
                 `((define-symbol-macro ,bound-symbol ,binding-form)))
             `((defvar ,bound-symbol)
               (defmacro ,(symbolicate 'with- class-name) (,v &body ,forms)
                 `(let ((,',bound-symbol ,,v))
                    ,@,forms))))
         ,@(iter
             (for (bound-name . accessor-name) in bound-accessors)
             ;; fixme; should these be inlined?
             (collect `(defun ,bound-name ()
                         (,accessor-name ,bound-symbol)))
             (collect `(defsetf ,bound-name () (,v)
                         `(setf (,',accessor-name ,',bound-symbol) ,,v))))))))
