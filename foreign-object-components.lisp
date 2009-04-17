;; Foreign object components: read and write
;; Liam Healy 2009-04-16 22:06:02EDT foreign-object-components.lisp
;; Time-stamp: <2009-04-17 13:23:03EDT foreign-object-components.lisp>
;; $Id: $

(in-package :fsbv)

(export 'foreign-object-components)

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

(defun foreign-object-components (object type &optional (index 0))
  "Find the components of the foreign object from its pointer."
  (funcall (get type 'foreign-object-components) object index))

(defun (setf foreign-object-components) (value object type &optional (index 0))
  (funcall (get type 'setf-foreign-object-components) value object index))
