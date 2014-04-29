struct-like-classes
===================

Common Lisp macro for classes defined with a DEFSTRUCT-like shorthand.

I use this in a few areas to save typing and to aid in live
development (changing struct inheritance or layout in a running system
with existing instances sometimes leads to badness only resolvable by
deleting and recreating the packages containing the struct definitions
-- possibly there are better ways to handle this, but this module is
how I currently do it).  I'm still undecided as to whether it's
actually advisable to (incompletely) reproduce the functionality
provided by DEFSTRUCT.

Thus, one can do:

```Common Lisp
(define-struct-like-class connection ()
  (server *server*)
  fd
  remote-peer
  remote-port
  forwarded-for)
```

instead of:


```Common lisp
(defclass connection ()
  (server :initform *server*
          :accessor connection-server)
  (fd :initform nil
      :accessor connection-fd)
  (remote-peer :initform nil
               :accessor connection-remote-peer)
  (remote-port :initform nil
               :accessor connection-remote-port)
  (forwarded-for :initform nil
                 :accessor connection-forwarded-for))

(defgeneric connection-p (obj)
  (:method ((obj connection)) t)
  (:method ((obj t)) nil))

(defun copy-connection (obj)
  (make-instance-with-same-slots obj))
```


There's also a DEFINE-STRUCT-LIKE-BOUND-CLASS helper macro, which
defines a struct-like class bound to a user-supplied symbol, a helper
macro for binding the symbol, and helper functions for accessing slots
of the currently-bound value.
