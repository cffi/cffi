;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; functions.lisp -- Calling foreign functions
;;;
;;; Copyright (C) 2009, 2010, 2011 Liam M. Healy  <lhealy@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi-fsbv)

(define-condition foreign-function-not-prepared (error)
  ((foreign-function-name
    :initarg :foreign-function-name :reader foreign-function-name))
  (:report
   (lambda (condition stream)
     (format stream "Foreign function ~a did not prepare correctly"
	     (foreign-function-name condition))))
  (:documentation
   "A condition that has been signalled by the FSBV library."))

(defun prepare-function
    (foreign-function-name return-type argument-types &optional (abi :default-abi))
  "Generate a closure that can be called on the Lisp objects and will return
   a Lisp object."
  (let* ((number-of-arguments (length argument-types))
	 (no-return-p (eql return-type :void))
	 (fo-symbols (loop for i from 0 below number-of-arguments
			   collect (make-symbol (format nil "ARG~d" i)))))
    `(let ((cif (cffi:foreign-alloc 'ffi-cif))
	   (ffi-argtypes (cffi:foreign-alloc :pointer :count ,number-of-arguments)))
       (setf ,@(loop for type in argument-types
		     for i from 0
		     append
		     `((cffi:mem-aref ffi-argtypes :pointer ,i)
		       (libffi-type-pointer ',type))))
       (unless
	   (eql :OK
		(prep-cif cif ,abi ,number-of-arguments
			  (libffi-type-pointer ',return-type)
			  ffi-argtypes))
	 (error
	  'foreign-function-not-prepared
	  :foreign-function-name ',foreign-function-name))
       (lambda (&rest args)
	 (cffi:with-foreign-objects
	     ,(append
	       (loop for type in argument-types
		     for symb in fo-symbols
		     collect `(,symb ',type))
	       `((argvalues :pointer ,number-of-arguments))
	       (unless no-return-p `((result ',return-type))))
	   ,@(loop
	       for type in argument-types
	       for symb in fo-symbols
	       for i from 0
	       collect
	       `(cffi:convert-into-foreign-memory (nth args ,i) ',type ',symb))
	   (setf
	    ,@(loop for symb in fo-symbols
		    for i from 0
		    append
		    `((cffi:mem-aref argvalues :pointer ,i) ,symb))))
	 (call cif
	       (cffi:foreign-symbol-pointer ,foreign-function-name)
	       ,(if no-return-p '(cffi:null-pointer) 'result)
	       argvalues)
	 ,(unless no-return-p `(cffi:convert-from-foreign result ',return-type))))))

;; (FOREIGN-FUNCALL-FORM "gsl_complex_add_real" NIL '(COMPLEX C :DOUBLE R COMPLEX) NIL)
;; If there are any foreign structs in args or return,
;; defcfun should fbind the result of prepare-function
;; foreign-funcall should just funcall it

;; (prepare-function "gsl_complex_add_real" 'complex '(complex :double))

#|

;;;;;;;;;;;; OBSOLETE

(defun defcfun-args-from-ff-args (arguments)
  "Convert the argument format from foreign-funcall to defcfun form.
   Returns a list of input arguments, and the return type."
  (values 
   (loop for (type symbol) on (butlast arguments) by #'cddr
      collect (list symbol type))
   (first (last arguments))))

(defmacro foreign-funcall (name-and-options &rest arguments)
  "Call the foreign function with or without structs-by-value."
  (multiple-value-bind (arguments-symbol-type return-type)
      (defcfun-args-from-ff-args arguments)
    (let ((name (name-from-name-and-options name-and-options)))
      (if (or (defined-type-p return-type)
	      (some 'defined-type-p (mapcar 'second arguments-symbol-type)))
	  `(funcall
	    ,(if (symbolp name)
		 `(get ',name 'prepared)
		 (prepare-function
		  ;; We do not use the "options" in name-and-options yet
		  name
		  return-type
		  (mapcar 'second arguments-symbol-type)))
	    ,@(mapcar 'first arguments-symbol-type))
	  ;; If there are no call or return by value structs, simply use
	  ;; cffi:foreign-funcall.
	  `(cffi:foreign-funcall ,name-and-options ,@arguments)))))

(defmacro defcfun (name-and-options return-type &body args)
  "Define a Lisp function that calls a foreign function.
   If the specified Lisp name has no home package (apparently
   uninterned), then the Lisp function is not made, but property
   'prepared for the function symbol is bound to the prepared
   function, through which the foreign function can be called."
  (multiple-value-bind (lisp-name foreign-name foreign-options)
      (cffi::parse-name-and-options name-and-options)
    (declare (ignore foreign-options))
    (let ((docstring (when (stringp (car args)) (pop args)))
	  (argsymbs (mapcar 'first args))
	  (set-property
	   `(setf (get ',lisp-name 'prepared)
		  ,(prepare-function
		    foreign-name return-type (mapcar 'second args)))))
      (if (symbol-package lisp-name)
	  `(progn
	     ,set-property
	     (defun ,lisp-name ,argsymbs
	       ,@(if docstring (list docstring))
	       (funcall (get ',lisp-name 'prepared) ,@argsymbs)))
	  ;; The symbol used for the lisp-name is apparently
	  ;; uninterned, so don't bother with the defun, because it
	  ;; could never be referenced.
	  set-property))))
|#
