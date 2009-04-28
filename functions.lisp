;; Calling foreign functions
;; Liam Healy 2009-04-17 13:04:15EDT functions.lisp
;; Time-stamp: <2009-04-27 22:50:05EDT functions.lisp>
;; $Id: $

(in-package :fsbv)

(export '(foreign-funcall))

;;; foreign-funcall args are (type arg ...)
;;; defcfun args are ((arg type) ...)
;;; This macro assumes defcfun style

(define-condition libffi-not-prepared (error)
  ((foreign-function-name
    :initarg :foreign-function-name :reader foreign-function-name))
  (:report
   (lambda (condition stream)
     (format stream "Foreign function ~a did not prepare correctly"
	     (foreign-function-name condition))))
  (:documentation
   "A condition that has been signalled by the FSBV library."))

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
	 (error 'libffi-not-prepared :foreign-function-name ',foreign-function-name))
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
