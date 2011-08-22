;; Foreign object components: read and write
;; Liam Healy 2009-04-16 22:06:02EDT foreign-object-components.lisp
;; Time-stamp: <2010-11-27 21:19:38EST foreign-object-components.lisp>
;; $Id: $

(in-package :fsbv)

(export '(defcenum-aux defsynonym))

(defmacro defcenum-aux (name &optional (base-type :int))
  "Auxiliary definitions for the enumation type named."
  `(setf
    (libffi-type-pointer ,name)
    (libffi-type-pointer ,base-type)
    (get ',name 'foreign-object-components)
    (lambda (object &optional (index 0))
      (cffi:foreign-enum-keyword
       ',name
       (cffi:mem-aref object ',name index)))
    (get ',name 'setf-foreign-object-components)
    (lambda (value object &optional (index 0))
      (setf (cffi:mem-aref object ',name index)
	    (cffi:foreign-enum-value ',name value)))))

(defmacro defsynonym (name type)
  "Define a new name for an existing type."
  `(setf
     (libffi-type-pointer ,name)
     (libffi-type-pointer ,type)
     (get ',name 'foreign-object-components)
     (get ',type 'foreign-object-components)
     (get ',name 'setf-foreign-object-components)
     (get ',type 'setf-foreign-object-components)))

