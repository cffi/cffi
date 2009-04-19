;; Foreign object components: read and write
;; Liam Healy 2009-04-16 22:06:02EDT foreign-object-components.lisp
;; Time-stamp: <2009-04-18 22:16:00EDT foreign-object-components.lisp>
;; $Id: $

(in-package :fsbv)

(export '(object with-foreign-objects))

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
(def-foc-direct :float)
(def-foc-direct :pointer)
(def-foc-direct :int8)
(def-foc-direct :int16)
(def-foc-direct :int32)
(def-foc-direct :int64)
(def-foc-direct :int)
(def-foc-direct :uint8)
(def-foc-direct :uint16)
(def-foc-direct :uint32)
(def-foc-direct :uint64)
(def-foc-direct :uint)
(def-foc-direct :char)
(def-foc-direct :unsigned-char)
(def-foc-direct :short)
(def-foc-direct :unsigned-short)
(def-foc-direct :long)
(def-foc-direct :unsigned-long)
(def-foc-direct :uchar)
(def-foc-direct :ushort)
(def-foc-direct :uint)
(def-foc-direct :ulong)

(defun object (foreign-object type &optional (index 0))
  "Create the CL object from the foreign object."
  (funcall (get type 'foreign-object-components) foreign-object index))

(defun (setf object) (value foreign-object type &optional (index 0))
  "Set the foreign object from the CL object contents."
  (funcall
   (get type 'setf-foreign-object-components)
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
