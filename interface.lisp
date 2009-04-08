;; User interface for making definitions
;; Liam Healy 2009-04-07 22:42:15EDT interface.lisp
;; Time-stamp: <2009-04-07 22:54:10EDT interface.lisp>
;; $Id: $

(in-package :fsbv)

;;; These macros are designed to make the interface to functions that
;;; get and/or return structs as transparent as possible, mimicking
;;; the CFFI definitions.

(defmacro defcstruct (name-and-options &body fields)
  `(cffi:defcstruct ,name-and-options ,@fields))
