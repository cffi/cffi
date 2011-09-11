;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cstruct.lisp --- Hook to defcstruct
;;;
;;; Copyright (C) 2009, 2010, Liam Healy  <lhealy@common-lisp.net>
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

;;; The hook defcstruct-hook is provided to add definitions need to
;;; use structures by value in function calls.  It will be called when
;;; defcstruct is expanded, inserting some forms at the end.

;;; Potential efficiency improvement: when a field has count > 1,
;;; define a pointer to the first element, and reference from that,
;;; instead of recomputing the pointer each element.

(defun lookup-type (symbol)
  (or `(libffi-type-pointer ,symbol)
      (error "Element type ~a is not known to libffi." symbol)))

(defun name-from-name-and-options (name-and-options)
  (if (listp name-and-options)
      (first name-and-options)
      name-and-options))

(defun option-from-name-and-options (name-and-options option default)
  (if (listp name-and-options)
      (getf (rest name-and-options) option default)
      default))

(defun field-count (field &optional (default 1))
  (getf field :count default))

(defun iterate-foreign-structure (fields form)
  "Iterate over the foreign structure, generating forms
   with form-function, a function of field, fn and gn.
   The argument fn is the count within the field, and
   gn is the overall count from 0."
  (loop for field in fields with gn = 0
     append
     (loop for fn from 0 below (field-count field)
	append
	(prog1
	    (funcall form field fn gn)
	  (incf gn)))))

(defun cstruct-libffi-hook (name-and-options &rest fields)
  "A function to produce forms in defcstruct to define the struct to
  CFFI and to libffi simultaneously."
  (let ((total-number-of-elements (apply '+ (mapcar 'field-count fields)))
	(name (name-from-name-and-options name-and-options)))
    `((setf (libffi-type-pointer ,name)
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
	      ptr)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cffi::*defcstruct-hook* 'cstruct-libffi-hook))
