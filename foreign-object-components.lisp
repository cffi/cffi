;; Foreign object components: read and write
;; Liam Healy 2009-04-16 22:06:02EDT foreign-object-components.lisp
;; Time-stamp: <2009-04-16 22:50:18EDT foreign-object-components.lisp>
;; $Id: $

(in-package :fsbv)

(export #:foreign-object-components)

(defmacro def-foc-direct (type)
  "Define the foreign object components reader and writer, assuming
   the cffi:mem-aref works on them."
  `(setf (get ',type 'foreign-object-components)
	 (lambda (object &optional (index 0))
	   (cffi:mem-aref object ',type index))
	 (get ',type 'setf-foreign-object-components)
	 (lambda (value object &optional (index 0))
	   (setf (cffi:mem-aref object ',type index) value))))

(def-foc-direct :double)
(def-foc-direct :int)

(defun foreign-object-components (object type &optional (index 0))
  "Find the components of the foreign object from its pointer."
  (funcall (get type 'foreign-object-components) object index))

(defun (setf foreign-object-components) (value object type &optional (index 0))
  (funcall (get type 'setf-foreign-object-components) value object index))

;;; This should be generalized to walk any structure, and then be part
;;; of the defcstruct expansion.
(setf (get 'complex 'foreign-object-components)
      (lambda (object &optional (index 0))
	(declare (ignore index))
	(let ((fp (cffi:foreign-slot-value object 'complex 'dat)))
	  (list 
	   (foreign-object-components fp :double 0)
	   (foreign-object-components fp :double 1))))
      (get 'complex 'setf-foreign-object-components)
      (lambda (value object &optional (index 0))
	(declare (ignore index))
	(let ((fp (cffi:foreign-slot-value object 'complex 'dat)))
	  (setf (foreign-object-components fp :double 0)
		(first value)
		(foreign-object-components fp :double 1)
		(second value))
	  value)))
