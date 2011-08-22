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

(in-package :fsbv)

(export '(defined-type-p))

;;; The hook defcstruct-hook is provided to add definitions need to
;;; use structures by value in function calls.  It will be called when
;;; defcstruct is expanded, inserting some forms at the end.

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

(defun defcstruct-hook (name-and-options &rest fields)
  "A function to produce forms in defcstruct to define the struct to
  CFFI and to libffi simultaneously."
  (let ((total-number-of-elements (apply '+ (mapcar 'field-count fields)))
	(name (name-from-name-and-options name-and-options)))
    (pushnew name *libffi-struct-defs*)
    `((pushnew ',name *libffi-struct-defs*)
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
	      ptr)
	    (get ',name 'foreign-object-components)
	    (lambda (object &optional (index 0))
	      (,(option-from-name-and-options name-and-options :constructor 'list)
		,@(iterate-foreign-structure
		   fields
		   (lambda (field fn gn)
		     (declare (ignore gn))
		     `(,(structure-slot-form field name fn))))))
	    (get ',name 'setf-foreign-object-components)
	    (lambda (value object &optional (index 0))
	      (setf
	       ,@(iterate-foreign-structure
		  fields
		  (lambda (field fn gn)
		    `(,(structure-slot-form field name fn)
		       ,(let ((decon
			       (option-from-name-and-options
				name-and-options :deconstructor 'elt)))
			     (if (listp decon)
				 `(,(nth gn decon) value)
				 `(,decon value ,gn))))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf cffi::*defcstruct-hook* 'defcstruct-hook))
