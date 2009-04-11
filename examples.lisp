;; Examples of using FSBV
;; Liam Healy 2009-04-07 22:13:34EDT examples.lisp
;; Time-stamp: <2009-04-11 15:17:21EDT examples.lisp>
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
(defun complex-abs (complex-number)
  (cffi:with-foreign-objects ((argument 'complex))
    (setf (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 0)
	  (realpart complex-number)
	  (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 1)
	  (imagpart complex-number))
    (foreign-funcall "gsl_complex_abs" complex argument :double)))

;;; gsl_complex_conjugate: an example of a function that takes a complex
;;; number and returns another complex number
(defun complex-conjugate (complex-number)
  (cffi:with-foreign-objects ((argument 'complex))
    (setf (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 0)
	  (realpart complex-number)
	  (cffi:mem-aref (cffi:foreign-slot-value argument 'complex 'dat)
			 :double 1)
	  (imagpart complex-number))
    (let ((ans
	   (foreign-funcall "gsl_complex_conjugate" complex argument complex)))
      (complex
       (cffi:mem-aref ans :double 0)
       (cffi:mem-aref ans :double 1)))))
