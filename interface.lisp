;; User interface for making definitions
;; Liam Healy 2009-04-07 22:42:15EDT interface.lisp
;; Time-stamp: <2009-04-09 22:59:38EDT interface.lisp>
;; $Id: $

(in-package :fsbv)

;;; These macros are designed to make the interface to functions that
;;; get and/or return structs as transparent as possible, mimicking
;;; the CFFI definitions.

(defun lookup-type (symbol)
  (or `(libffi-type-pointer ,symbol)
      (error "Element type ~a is not known to libffi." symbol)))

(defun field-count (field)
  (getf field :count 1))

(defmacro defcstruct (name-and-options &body fields)
  "A macro to define the struct to CFFI and to libffi simultaneously.
   Syntax is exactly that of cffi:defcstruct."
  (let ((total-number-of-elements (apply '+ (mapcar 'field-count fields))))
    `(progn
       (cffi:defcstruct ,name-and-options ,@fields)
       (setf (libffi-type-pointer
	      ,(if (listp name-and-options)
		   (first name-and-options)
		   name-and-options))
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

(defmacro libffi-function-wrapper (return-type arguments)
  "Create the libffi objects necessary to call the function of the
   given arguments, which is a list of (arg type) lists."
  (let ((number-of-arguments (length arguments)))
    `(cffi:with-foreign-objects
	 ((cif 'ffi-cif)
	  (argtypes :pointer ,number-of-arguments)
	  (argvalues :pointer ,number-of-arguments)
	  (result ,return-type))
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
	     (cffi:foreign-symbol-pointer "gsl_complex_abs")
	     result
	     argvalues)
       (cffi:mem-aref result ,return-type))))))

;;; (libffi-function-wrapper :double ((in complex)))
