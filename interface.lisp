;; User interface for making definitions
;; Liam Healy 2009-04-07 22:42:15EDT interface.lisp
;; Time-stamp: <2009-04-09 00:29:10EDT interface.lisp>
;; $Id: $

(in-package :fsbv)

;;; These macros are designed to make the interface to functions that
;;; get and/or return structs as transparent as possible, mimicking
;;; the CFFI definitions.

(defmacro libffi-type-pointer (symbol) `(get ',symbol 'type-pointer))

;;; Types supported by CFFI:
;;; http://common-lisp.net/project/cffi/manual/html_node/Built_002dIn-Types.html#Built_002dIn-Types
;;; Assign these more accurately?
(defparameter *cffi-built-in-types*
  '((:char . +pointer-type-sint8+)
    (:unsigned-char . +pointer-type-uint8+)
    (:short . +pointer-type-sint16+)
    (:unsigned-short . +pointer-type-uint16+)
    (:int . +pointer-type-sint32+)
    (:unsigned-int  . +pointer-type-uint32+)
    (:long . +pointer-type-sint64+)
    (:unsigned-long . +pointer-type-uint64+)
    (:uchar . +pointer-type-uint8+)
    (:ushort . +pointer-type-uint16+)
    (:uint  . +pointer-type-uint32+)
    (:ulong . +pointer-type-uint64+)
    ;; (:llong)
    ;; (:ullong)
    (:int8 . +pointer-type-sint8+)
    (:uint8 . +pointer-type-uint8+)
    (:int16 . +pointer-type-sint16+)
    (:uint16 . +pointer-type-uint16+)
    (:int32 . +pointer-type-sint32+)
    (:uint32 . +pointer-type-uint32+)
    (:int64 . +pointer-type-sint64+)
    (:uint64 . +pointer-type-uint64+)
    (:float . +pointer-type-float+)
    (:double . +pointer-type-double+)
    (:long-double . +pointer-type-longdouble+)
    (:pointer . +pointer-type-pointer+)
    (:void)))

(defun lookup-type (symbol)
  (or (rest (assoc symbol *cffi-built-in-types*))
      `(libffi-type-pointer ,symbol)
      (error "Element type ~a is not known to libffi." symbol)))

(defun field-count (field)
  (getf field :count 1))

(defmacro defcstruct (name-and-options &body fields)
  "A macro to define the struct to CFFI and to libffi simultaneously.
   Syntax is exactly that of cffi:defcstruct."
  `(progn
     (cffi:defcstruct ,name-and-options ,@fields)
     (setf (libffi-type-pointer
	    ,(if (listp name-and-options)
		 (first name-and-options)
		 name-and-options))
	   (let ((ptr (cffi:foreign-alloc 'ffi-type))
		 (elements (cffi:foreign-alloc
			    :pointer
			    :count
			    ,(1+ (apply '+ (mapcar 'field-count fields))))))
	     (setf
	      ;; The elements
	      ,@(loop for field in fields with gn = 0
		   append
		   (loop for fn from 0 below (field-count field)
		      append
		      (prog1
			  (list
			   `(cffi:mem-aref elements :pointer ,gn)
			   (lookup-type (second field)))
			(incf gn))))
	      ;; The ffi-type
	      (cffi:foreign-slot-value ptr 'ffi-type 'size) 0
	      (cffi:foreign-slot-value ptr 'ffi-type 'alignment) 0
	      (cffi:foreign-slot-value ptr 'ffi-type 'type) +type-struct+
	      (cffi:foreign-slot-value ptr 'ffi-type 'elements) elements)
	     ptr))))
