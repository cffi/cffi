;; Examples of using FSBV
;; Liam Healy 2009-04-07 22:13:34EDT examples.lisp
;; Time-stamp: <2009-04-13 14:04:55EDT examples.lisp>
;; $Id: $

(in-package :fsbv)

;;; These examples are based on GSL functions using and returning complex numbers
;;; http://www.gnu.org/software/gsl/manual/html_node/Properties-of-complex-numbers.html
;;; http://www.gnu.org/software/gsl/manual/html_node/Complex-arithmetic-operators.html

;;; Install the GSL libraries, load this file, then try
;;; (complex-abs #c(1.0d0 2.0d0))
;;; (complex-conjugate #c(1.0d0 2.0d0))
;;; (complex-add #c(1.0d0 2.0d0) #c(3.0d0 4.0d0))
;;; (complex-add-real #c(1.0d0 2.0d0) 3.0d0)

;;; Load the libraries
(cffi:load-foreign-library #+unix "libgslcblas.so")
(cffi:load-foreign-library #+unix "libgsl.so")

;;; Define the foreign struct; see /usr/include/gsl/gsl_complex.h
(defcstruct complex (dat :double :count 2))

(defmacro foreign-realpart (complex)
  "Extract the real part of the foreign complex struct."
  `(cffi:mem-aref (cffi:foreign-slot-value ,complex 'complex 'dat)
		  :double 0))

(defmacro foreign-imagpart (complex)
  "Extract the imaginary part of the foreign complex struct."
  `(cffi:mem-aref (cffi:foreign-slot-value ,complex 'complex 'dat)
		  :double 1))

;;; gsl_complex_abs: an example of a function that takes a complex
;;; number and returns a double-float
(defun complex-abs (complex-number)
  (cffi:with-foreign-objects ((argument 'complex))
    (setf (foreign-realpart argument) (realpart complex-number)
	  (foreign-imagpart argument) (imagpart complex-number))
    (foreign-funcall "gsl_complex_abs" complex argument :double)))

;;; gsl_complex_conjugate: an example of a function that takes a complex
;;; number and returns another complex number
(defun complex-conjugate (complex-number)
  (cffi:with-foreign-objects ((argument 'complex))
    (setf (foreign-realpart argument) (realpart complex-number)
	  (foreign-imagpart argument) (imagpart complex-number))
    (let ((ans (foreign-funcall "gsl_complex_conjugate"
				complex argument complex)))
      (complex
       (foreign-realpart ans)
       (foreign-imagpart ans)))))

;;; gsl_complex_add: an example of a function that takes two complex
;;; numbers and returns another complex number
(defun complex-add (c1 c2)
  (cffi:with-foreign-objects ((arg1 'complex) (arg2 'complex))
    (setf (foreign-realpart arg1) (realpart c1)
	  (foreign-imagpart arg1) (imagpart c1)
	  (foreign-realpart arg2) (realpart c2)
	  (foreign-imagpart arg2) (imagpart c2))
    (let ((ans (foreign-funcall "gsl_complex_add"
				complex arg1 complex arg2 complex)))
      (complex
       (foreign-realpart ans)
       (foreign-imagpart ans)))))

;;; gsl_complex_add_real: an example of a function that takes one complex
;;; number and one real number and returns another complex number
(defun complex-add-real (c1 real)
  (cffi:with-foreign-objects ((arg1 'complex) (arg2 :double))
    (setf (foreign-realpart arg1) (realpart c1)
	  (foreign-imagpart arg1) (imagpart c1)
	  (cffi:mem-aref arg2 :double) real)
    (let ((ans (foreign-funcall "gsl_complex_add_real"
				complex arg1 :double arg2 complex)))
      (complex
       (foreign-realpart ans)
       (foreign-imagpart ans)))))
