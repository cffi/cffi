;; User interface for making definitions
;; Liam Healy 2009-04-07 22:42:15EDT interface.lisp
;; Time-stamp: <2009-04-11 22:19:53EDT interface.lisp>
;; $Id: $

(in-package :fsbv)

(export '(defcstruct foreign-funcall))

;;; These macros are designed to make the interface to functions that
;;; get and/or return structs as transparent as possible, mimicking
;;; the CFFI definitions.

(defun lookup-type (symbol)
  (or `(libffi-type-pointer ,symbol)
      (error "Element type ~a is not known to libffi." symbol)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *libffi-struct-defs* nil))

(defun field-count (field)
  (getf field :count 1))

(defun name-from-name-and-options (name-and-options)
  (if (listp name-and-options)
      (first name-and-options)
      name-and-options))

(defmacro defcstruct (name-and-options &body fields)
  "A macro to define the struct to CFFI and to libffi simultaneously.
   Syntax is exactly that of cffi:defcstruct."
  (let ((total-number-of-elements (apply '+ (mapcar 'field-count fields)))
	(name (name-from-name-and-options name-and-options)))
    (pushnew name *libffi-struct-defs*)
    `(progn
       (cffi:defcstruct ,name-and-options ,@fields)
       (pushnew ',name *libffi-struct-defs*)
       (setf (libffi-type-pointer ,name)
	     (let ((ptr (cffi:foreign-alloc 'ffi-type))
		   (elements (cffi:foreign-alloc
			      :pointer
			      :count
			      ,(1+ total-number-of-elements))))
	       (setf
		;; The elements
		,@(loop for field in fields with gn = 0
		     append
		     (loop for fn from 0 below (field-count field)
			append
			(prog1
			    (list
			     `(cffi:mem-aref elements :pointer ,gn)
			     (lookup-type (second field)))
			  (incf gn))))
		(cffi:mem-aref elements :pointer ,total-number-of-elements)
		(cffi:null-pointer)
		;; The ffi-type
		(cffi:foreign-slot-value ptr 'ffi-type 'size) 0
		(cffi:foreign-slot-value ptr 'ffi-type 'alignment) 0
		(cffi:foreign-slot-value ptr 'ffi-type 'type) +type-struct+
		(cffi:foreign-slot-value ptr 'ffi-type 'elements) elements)
	       ptr)))))

;;; foreign-funcall args are (type arg ...)
;;; defcfun args are ((arg type) ...)
;;; This macro assumes defcfun style

(defmacro libffi-function-wrapper
    (foreign-function-name return-type arguments)
  "Create the libffi objects necessary to call the function of the
   given arguments, which is a list of (arg type) lists."
  (let ((number-of-arguments (length arguments))
	(no-return-p (member return-type '(:void))))
    `(cffi:with-foreign-objects
	 ((cif 'ffi-cif)
	  (argtypes :pointer ,number-of-arguments)
	  (argvalues :pointer ,number-of-arguments)
	  ,@(unless no-return-p `((result ',return-type))))
       (setf ,@(loop for arg in arguments
		  for argc from 0
		  append
		  `((cffi:mem-aref argtypes :pointer ,argc)
		    (libffi-type-pointer ,(second arg))
		    (cffi:mem-aref argvalues :pointer ,argc)
		    ,(first arg))))
       (unless
	   (eql :OK
		(prep-cif cif :default-abi ,number-of-arguments
			  (libffi-type-pointer ,return-type)
			  argtypes))
	 (error "Function did not prepare correctly."))
       (call cif
	     (cffi:foreign-symbol-pointer ,foreign-function-name)
	     ,(if no-return-p '(cffi:null-pointer) 'result)
	     argvalues)
       ,(unless no-return-p
		`(cffi:mem-aref result ',return-type)))))

(defmacro foreign-funcall (name-and-options &rest arguments)
  "Call the foreign function with or without structs-by-value."
  (let ((arguments-symbol-type
	 (loop for (type symbol) on (butlast arguments) by #'cddr
	    collect (list symbol type)))
	(return-type (first (last arguments))))
    (if (or (member return-type *libffi-struct-defs*)
	    (intersection *libffi-struct-defs*
			  (mapcar 'second arguments-symbol-type)))
	`(libffi-function-wrapper
	  ;; We do not use the "options" in name-and-options yet
	  ,(name-from-name-and-options name-and-options)
	  ,return-type ,arguments-symbol-type)
	;; If there are no call or return by value structs, simply use
	;; cffi:foreign-funcall.
	`(cffi:foreign-funcall ,name-and-options ,@arguments))))
