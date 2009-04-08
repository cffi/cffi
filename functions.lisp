;; Interface to libffi functions
;; Liam Healy 2009-04-06 21:14:55EDT functions.lisp
;; Time-stamp: <2009-04-07 21:50:30EDT functions.lisp>

(in-package :fsbv)

;;; See file:///usr/share/doc/libffi-dev/html/The-Basics.html#The-Basics

(cffi:defcfun ("ffi_prep_cif" prep-cif) status
    (ffi-cif :pointer)
    (ffi-abi abi)
    (nargs :uint)
    (rtype :pointer)
    (argtypes :pointer))

(cffi:defcfun ("ffi_call" call) :void
    (ffi-cif :pointer)
    (function :pointer)
    (rvalue :pointer)
    (avalues :pointer))

(load-foreign-library #+unix "libgslcblas.so")
(load-foreign-library #+unix "libgsl.so")

(defvar +pointer-type-complex+
  ;; See file:///usr/share/doc/libffi-dev/html/Structures.html#Structures
  (let ((ptr (cffi:foreign-alloc 'ffi-type))
	(complex (cffi:foreign-alloc :pointer :count 3)))
    (setf
     ;; The elements
     (cffi:mem-aref complex :pointer 0) +pointer-type-double+
     (cffi:mem-aref complex :pointer 1) +pointer-type-double+
     (cffi:mem-aref complex :pointer 2) (cffi:null-pointer)
     ;; The ffi-type
     (cffi:foreign-slot-value ptr 'ffi-type 'size) 0
     (cffi:foreign-slot-value ptr 'ffi-type 'alignment) 0
     (cffi:foreign-slot-value ptr 'ffi-type 'type) +type-struct+
     (cffi:foreign-slot-value ptr 'ffi-type 'elements) complex)
    ptr))

;;; From /usr/include/gsl/gsl_complex.h
(cffi:defcstruct complex
  (dat :double :count 2))

(defun test-call (complex-number)
  (with-foreign-objects
      ((cif 'ffi-cif)
       (argtypes :pointer 1)
       (argvalues :pointer 1)
       (result :double)
       (numb 'complex))
    (setf (cffi:mem-aref argtypes :pointer 0) +pointer-type-complex+)
    (setf (cffi:mem-aref (cffi:foreign-slot-value numb 'complex 'dat) :double 0)
	  (realpart complex-number)
	  (cffi:mem-aref (cffi:foreign-slot-value numb 'complex 'dat) :double 1)
	  (imagpart complex-number))
    (setf (cffi:mem-aref argvalues :pointer 0)
	  numb)
    (when (eql
	   :OK
	   (prep-cif cif :default-abi 1 +pointer-type-double+ argtypes))
      (call cif
	    (cffi:foreign-symbol-pointer "gsl_complex_abs")
	    result
	    argvalues)
      (cffi:mem-aref result :double))))
