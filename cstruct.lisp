;; Defining C structures.
;; Liam Healy 2009-04-07 22:42:15EDT interface.lisp
;; Time-stamp: <2010-11-27 18:28:14EST cstruct.lisp>
;; $Id: $

(in-package :fsbv)

(export '(defcstruct defined-type-p))

;;; These macros are designed to make the interface to functions that
;;; get and/or return structs as transparent as possible, mimicking
;;; the CFFI definitions.

;;; Potential efficiency improvement: when a filed has count > 1,
;;; define a pointer to the first element, and reference from that,
;;; instead of recomputing the pointer each element.

(defun lookup-type (symbol)
  (or `(libffi-type-pointer ,symbol)
      (error "Element type ~a is not known to libffi." symbol)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *libffi-struct-defs* nil))

(defun defined-type-p (name)
  "This structure has been defined for call-by-value."
  (member name *libffi-struct-defs*))

(defmacro defcstruct (name-and-options &body fields)
  "A macro to define the struct to CFFI and to libffi simultaneously.
   Syntax is exactly that of cffi:defcstruct."
  (let ((total-number-of-elements (apply '+ (mapcar 'field-count fields)))
	(name (name-from-name-and-options name-and-options)))
    (pushnew name *libffi-struct-defs*)
    `(progn
       (cffi:defcstruct
	   ,(name-from-name-and-options name-and-options)
	 ,@fields)
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (pushnew ',name *libffi-struct-defs*))
       (setf (libffi-type-pointer ,name)
	     (let ((ptr (cffi:foreign-alloc 'ffi-type))
		   (elements (cffi:foreign-alloc
			      :pointer
			      :count
			      ,(1+ total-number-of-elements))))
	       (setf
		;; The elements
		,@(iterate-foreign-structure
		   fields
		   (lambda (field fn gn)
		     (declare (ignore fn))
		     (list
		      `(cffi:mem-aref elements :pointer ,gn)
		      (lookup-type (second field)))))
		(cffi:mem-aref elements :pointer ,total-number-of-elements)
		(cffi:null-pointer)
		;; The ffi-type
		(cffi:foreign-slot-value ptr 'ffi-type 'size) 0
		(cffi:foreign-slot-value ptr 'ffi-type 'alignment) 0
		(cffi:foreign-slot-value ptr 'ffi-type 'type) +type-struct+
		(cffi:foreign-slot-value ptr 'ffi-type 'elements) elements)
	       ptr))
       (defconvert ,name-and-options ,@fields)
       ',name)))
