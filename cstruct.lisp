;; Defining C structures.
;; Liam Healy 2009-04-07 22:42:15EDT interface.lisp
;; Time-stamp: <2009-04-25 17:07:35EDT cstruct.lisp>
;; $Id: $

(in-package :fsbv)

(export '(defcstruct))

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

(defun field-count (field)
  (getf field :count 1))

(defun name-from-name-and-options (name-and-options)
  (if (listp name-and-options)
      (first name-and-options)
      name-and-options))

(defun option-from-name-and-options (name-and-options option default)
  (if (listp name-and-options)
      (getf (rest name-and-options) option default)
      default))

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
       (pushnew ',name *libffi-struct-defs*)
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
	       (declare (ignore index))
	       (,(option-from-name-and-options name-and-options :constructor 'list)
		 ,@(iterate-foreign-structure
		    fields
		    (lambda (field fn gn)
		      (declare (ignore gn))
		      (list
		       `(object
			 (cffi:foreign-slot-value object ',name ',(first field))
			 ,(second field) ,fn))))))
	     (get ',name 'setf-foreign-object-components)
	     (lambda (value object &optional (index 0))
	       (declare (ignore index))
	       (setf
		,@(iterate-foreign-structure
		   fields
		   (lambda (field fn gn)
		     `((object
			(cffi:foreign-slot-value object ',name ',(first field))
			,(second field) ,fn)
		       ,(let ((decon
			       (option-from-name-and-options
				name-and-options :deconstructor 'elt)))
			     (if (listp decon)
				 `(,(nth gn decon) value)
				 `(,decon value ,gn)))))))))
       ',name)))
