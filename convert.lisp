;; Convert foreign objects to and from Lisp
;; Liam Healy 2010-11-27 17:46:55EST convert.lisp
;; Time-stamp: <2011-08-18 22:47:49EDT convert.lisp>

;;; This file depends on nothing else and could be split off from the
;;; rest of FSBV, for anyone that wanted to do conversions to and from
;;; foreign objects without calling them or returning them by value.

(in-package :fsbv)

(export '(defconvert object with-foreign-objects
	  converter-defined-p convert-from-pointer))

(defvar *converter-types* nil)
(defun converter-defined-p (name)
  "This structure has been defined for call-by-value."
  (member name *converter-types*))

(defmacro defconvert (name-and-options &body fields)
  "A macro to define the converters to and from the foreign struct.
   Syntax is exactly that of cffi:defcstruct."
  (let ((name (name-from-name-and-options name-and-options)))
    (pushnew name *converter-types*)
    `(progn
       (pushnew ',name *converter-types*)
       (setf
	(get ',name 'foreign-object-components)
	,(convert-function name-and-options fields)
	(get ',name 'setf-foreign-object-components)
	,(convert-setf-function name-and-options fields)))))

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

(defun structure-slot-form (field structure-name fn)
  "A form for getting or setting the foreign slot value.
   The variables 'object and 'index are captured."
  (let ((form
	 `(cffi:foreign-slot-value
	   (cffi:mem-aref object ',structure-name index)
	   ',structure-name ',(first field))))
    (if (field-count field nil)		; aggregate slot
	`(object ,form ,(second field) ,fn)
	;; simple slot
	form)))

(defun convert-function (name-and-options fields)
  "Create a function that converts an object from foreign to CL object."
  `(lambda (object &optional (index 0))
     (,(option-from-name-and-options name-and-options :constructor 'list)
       ,@(iterate-foreign-structure
	  fields
	  (lambda (field fn gn)
	    (declare (ignore gn))
	    `(,(structure-slot-form
		field
		(name-from-name-and-options name-and-options)
		fn)))))))

(defun convert-setf-function (name-and-options fields)
  "Create a function that converts an object from  CL object to foreign."
  `(lambda (value object &optional (index 0))
     (setf
      ,@(iterate-foreign-structure
	 fields
	 (lambda (field fn gn)
	   `(,(structure-slot-form
	       field
	       (name-from-name-and-options name-and-options)
	       fn)
	      ,(let ((decon
		      (option-from-name-and-options
		       name-and-options :deconstructor 'elt)))
		 (if (listp decon)
		     `(,(nth gn decon) value)
		     `(,decon value ,gn)))))))))

(defun object (foreign-object type &optional (index 0))
  "Create the CL object from the foreign object."
  (funcall (or (get type 'foreign-object-components)
	       (error "No function defined to convert ~a to Lisp from foreign." type))
	   foreign-object index))

(defun (setf object) (value foreign-object type &optional (index 0))
  "Set the foreign object value from the CL object contents."
  (funcall
   (or (get type 'setf-foreign-object-components)
       (error "No function defined to convert ~a from Lisp to foreign." type))
   value foreign-object index))

(defmacro with-foreign-objects (bindings &body body)
  "For each binding (var type &optional initial-value), bind the
   variable to a foreign object whose contents is the same as the
   corresponding CL object."
  `(cffi:with-foreign-objects
       ,(mapcar (lambda (al) (subseq al 0 2)) bindings)
     (setf
      ,@(mapcan
	 (lambda (bnd)
	   (when (third bnd)
	     `((object ,(first bnd) ,(second bnd)) ,(third bnd))))
	 bindings))
     ,@body))

(defun convert-from-pointer (pointer type &optional (index 0))
  "Create a form that creates a CL object from the foreign pointer."
  (if (converter-defined-p type)
      `(object (cffi:mem-aref ,pointer ',type ,index) ',type)
      `(cffi:mem-aref ,pointer ',type ,index)))
