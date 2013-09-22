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

(in-package #:cffi)

(defvar *cif-table* (make-hash-table :test 'equal)
  "A hash table of foreign functions and pointers to the foreign cif (Call InterFace) structure for that function.")

(define-condition foreign-function-not-prepared (error)
  ((foreign-function-name
    :initarg :foreign-function-name :reader foreign-function-name))
  (:report
   (lambda (condition stream)
     (format stream "Foreign function ~a did not prepare correctly"
	     (foreign-function-name condition))))
  (:documentation
   "Preparation of foreign function did not succeed, according to return from libffi library."))

(defun prepare-function
    (foreign-function-name return-type argument-types &optional (abi :default-abi))
  "Generate or retrieve the CIF needed to call the function through libffi."
  (or (gethash foreign-function-name *cif-table*)
      (let* ((number-of-arguments (length argument-types))
             (cif (foreign-alloc '(:struct ffi-cif)))
             (ffi-argtypes (foreign-alloc :pointer :count number-of-arguments)))
        (loop for type in argument-types
              for i from 0
              do
                 (setf (mem-aref ffi-argtypes :pointer i)
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
      (foreign-free
       (foreign-slot-value ptr '(:struct ffi-cif) 'argument-types))
      (foreign-free ptr)
      (remhash foreign-function-name *cif-table*)
      foreign-function-name)))

(defun ffcall-body-libffi
    (function symbols return-type argument-types &optional pointerp (abi :default-abi))
  "A body of foreign-funcall calling the libffi function #'call (ffi_call)."
  (let ((number-of-arguments (length argument-types)))
    `(with-foreign-objects
         ((argvalues :pointer ,number-of-arguments)
          ,@(unless (eql return-type :void)
              `((result ',return-type))))
       (loop :for arg :in (list ,@symbols)
             :for count :from 0
             :do (setf (mem-aref argvalues :pointer count) arg))
       (call
        (prepare-function ,function ',return-type ',argument-types ',abi)
        ,(if pointerp
             function
             `(foreign-symbol-pointer ,function))
        ,(if (eql return-type :void) '(null-pointer) 'result)
        argvalues)
       ,(if (eql return-type :void)
            '(values)
            (if (typep (parse-type return-type) 'translatable-foreign-type)
                ;; just return the pointer so that expand-from-foreign
                ;; can apply translate-from-foreign
                'result
                ;; built-in types won't be translated by
                ;; expand-from-foreign, we have to do it here
                `(mem-aref result ',return-type))))))

(setf *foreign-structures-by-value* 'ffcall-body-libffi)

(pushnew :fsbv *features*)
