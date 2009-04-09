;; Examples of using FSBV
;; Liam Healy 2009-04-07 22:13:34EDT examples.lisp
;; Time-stamp: <2009-04-09 00:36:19EDT examples.lisp>
;; $Id: $

(in-package :fsbv)

;;; These examples are based on GSL functions using and returning complex numbers
;;; http://www.gnu.org/software/gsl/manual/html_node/Properties-of-complex-numbers.html
;;; http://www.gnu.org/software/gsl/manual/html_node/Complex-arithmetic-operators.html

;;; Load the libraries
(cffi:load-foreign-library #+unix "libgslcblas.so")
(cffi:load-foreign-library #+unix "libgsl.so")

;;; Define the foreign struct; see /usr/include/gsl/gsl_complex.h
(defcstruct complex
  (dat :double :count 2))

;;; gsl_complex_abs: an example of a function that takes a complex
;;; number and returns a double-float
(defun complex-in (complex-number)
  (cffi:with-foreign-objects
      ((cif 'ffi-cif)
       (argtypes :pointer 1)
       (argvalues :pointer 1)
       (result :double)
       (argument 'complex))
    (setf (cffi:mem-aref argtypes :pointer 0) (libffi-type-pointer complex)
	  (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 0)
	  (realpart complex-number)
	  (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 1)
	  (imagpart complex-number)
	  (cffi:mem-aref argvalues :pointer 0) argument)
    (when (eql :OK (prep-cif cif :default-abi 1 +pointer-type-double+ argtypes))
      (call cif
	    (cffi:foreign-symbol-pointer "gsl_complex_abs")
	    result
	    argvalues)
      (cffi:mem-aref result :double))))

;;; gsl_complex_conjugate: an example of a function that takes a complex
;;; number and returns another complex number
(defun complex-in-out (complex-number)
  (cffi:with-foreign-objects
      ((cif 'ffi-cif)
       (argtypes :pointer 1)
       (argvalues :pointer 1)
       (result 'complex)
       (argument 'complex))
    (setf (cffi:mem-aref argtypes :pointer 0) (libffi-type-pointer complex)
	  (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 0)
	  (realpart complex-number)
	  (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 1)
	  (imagpart complex-number)
	  (cffi:mem-aref argvalues :pointer 0) argument)
    (when (eql :OK (prep-cif cif :default-abi 1 (libffi-type-pointer complex) argtypes))
      (call cif
	    (cffi:foreign-symbol-pointer "gsl_complex_conjugate")
	    result
	    argvalues)
      (let ((res (cffi:foreign-slot-value result 'complex 'dat)))
	(complex
	 (cffi:mem-aref res :double 0)
	 (cffi:mem-aref res :double 1))))))
