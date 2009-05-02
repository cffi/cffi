;; Bulit-in types known to CFFI
;; Liam Healy 2009-05-02 13:43:08EDT cbuiltin.lisp
;; Time-stamp: <2009-05-02 14:06:54EDT cbuiltin.lisp>
;; $Id: $

;;; Handle built-in types; see
;;; http://common-lisp.net/project/cffi/manual/html_node/Built_002dIn-Types.html#Built_002dIn-Types

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun ffi-builtin-name (type)
  "The libffi string from the built-in name of the CFFI type."
  (let ((str (string-downcase type)))
    (format nil "ffi_type_~a~a"
	    (if (string= str "int" :end1 3) "s" "")
	    str))))

(defmacro defcbuiltin (type)
  "Define the foreign object components reader and writer, assuming
   the cffi:mem-aref works on them."
  `(setf (libffi-type-pointer ,type)
	 (cffi:foreign-symbol-pointer ,(ffi-builtin-name type))
	 (get ',type 'foreign-object-components)
	 (lambda (object &optional (index 0))
	   (cffi:mem-aref object ',type index))
	 (get ',type 'setf-foreign-object-components)
	 (lambda (value object &optional (index 0))
	   (setf (cffi:mem-aref object ',type index) value))))

(defcbuiltin :double)
(defcbuiltin :float)
(defcbuiltin :pointer)
(defcbuiltin :int8)
(defcbuiltin :int16)
(defcbuiltin :int32)
(defcbuiltin :int64)
(defcbuiltin :uint8)
(defcbuiltin :uint16)
(defcbuiltin :uint32)
(defcbuiltin :uint64)
(defcbuiltin :uint)

;;; Assign these more accurately?
(defsynonym :char :int8)
(defsynonym :uchar :uint8)
(defsynonym :unsigned-char :uint8)
(defsynonym :short :int16)
(defsynonym :ushort :uint16)
(defsynonym :unsigned-short :uint16)
(defsynonym :int :int32)
(defsynonym :uint :uint32)
(defsynonym :long :int64)
(defsynonym :ulong :uint64)
(defsynonym :unsigned-long :uint64)
