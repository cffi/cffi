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

(defvar *cif-table* (make-hash-table :test 'equal)
  "A hash table of foreign functions and pointers to the forign cif (Call InterFace) structure for that function.")

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
  "Generate or retrieve the CIF needed to call the function through libffi."
  (or (gethash foreign-function-name *cif-table*)
      (let* ((number-of-arguments (length argument-types))
             (cif (cffi:foreign-alloc '(:struct ffi-cif)))
             (ffi-argtypes (cffi:foreign-alloc :pointer :count number-of-arguments)))
        (loop for type in argument-types
              for i from 0
              do
                 (setf (cffi:mem-aref ffi-argtypes :pointer i)
                       (libffi-type-pointer (parse-type type))))
        (unless
            (eql :OK
                 (prep-cif cif abi number-of-arguments
                           (libffi-type-pointer (parse-type return-type))
                           ffi-argtypes))
          (error
           'foreign-function-not-prepared
           :foreign-function-name foreign-function-name))
        (setf (gethash foreign-function-name *cif-table*) cif)
        cif)))

(defun unprepare-function (foreign-function-name)
  "Remove prepared definitions for the named foreign function.  Returns foreign-function-name if function had been prepared, NIL otherwise."
  (let ((ptr (gethash foreign-function-name *cif-table*)))
    (when ptr
      (cffi:foreign-free
       (cffi:foreign-slot-value ptr '(:struct ffi-cif) 'argument-types))
      (cffi:foreign-free ptr)
      (remhash foreign-function-name *cif-table*)
      foreign-function-name)))

(defun callable-function
    (function return-type argument-types &optional pointerp (abi :default-abi))
  "Return a lambda that will call the libffi function #'call (ffi_call)."
  (let ((number-of-arguments (length argument-types)))
    `(lambda (&rest args)
       (cffi:with-foreign-objects
           ((argvalues :pointer ,number-of-arguments)
            (result ',return-type))
         (unwind-protect
              (progn
                (loop for arg in args
                      for type in ',argument-types
                      for count from 0
                      do (setf (cffi:mem-aref argvalues :pointer count)
                               (cffi:convert-to-foreign arg type)))
                ;; Make all the foreign objects, set the values then call
                (call
                 (prepare-function ,function ',return-type ',argument-types ',abi)
                 (if ,pointerp
                     ,function
                     (cffi:foreign-symbol-pointer ,function))
                 result
                 argvalues)
                ,(if (eql return-type :void)
                     '(values)
                     `(cffi:mem-aref result ',return-type)))
           (loop for type in ',argument-types
                 for count from 0
                 do (cffi:free-converted-object
                     (cffi:mem-aref argvalues :pointer count)
                     type nil)))))))

(setf *foreign-structures-by-value* 'callable-function)

#|

;;; Not ported yet

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
