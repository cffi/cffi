;; Examples of using CFFI-FSBV
;; Liam Healy 2009-04-07 22:13:34EDT examples.lisp
;; Time-stamp: <2011-09-21 23:32:16EDT examples.lisp>

(in-package :cffi)			; cffi-test ?  doesn't load

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

(defcstruct (complex-double :class complex-type)
 (real :double)
 (imag :double))

;;; Define the foreign struct; see /usr/include/gsl/gsl_complex.h
;;(defcstruct (complex :constructor complex :deconstructor (realpart imagpart))
;;  (dat :double :count 2))

(defmethod translate-into-foreign-memory ((value complex) (type complex-type) p)
  (with-foreign-slots ((real imag) p (:struct complex-double))
    (setf real (realpart value)
	  imag (imagpart value))))

(defmethod translate-from-foreign (p (type complex-type))
 (with-foreign-slots ((real imag) p (:struct complex-double))
   (complex real imag)))

(defmethod free-translated-object (value (p complex-type) freep)
  (declare (ignore freep))
  (foreign-free value))

(foreign-funcall
  "gsl_complex_abs"
  (:struct complex-double) #C(3.0d0 4.0d0)
  (:struct complex-double))

;;; Shouldn't this autoconvert?
(convert-from-foreign
 (foreign-funcall
  "gsl_complex_abs"
  (:struct complex-double) (convert-to-foreign #C(3.0d0 4.0d0) '(:struct complex-double))
  (:struct complex-double))
 '(:struct complex-double))

;;; gsl_complex_abs: an example of a function that takes a complex
;;; number and returns a double-float
(defcfun (complex-abs "gsl_complex_abs") :double
  "Find the absolute value of the complex number."
  (complex-number complex))

;;; gsl_complex_conjugate: an example of a function that takes a complex
;;; number and returns another complex number
(defcfun (complex-conjugate "gsl_complex_conjugate") complex
  "Find the complex conjugate of the given complex number."
  (c complex))

;;; gsl_complex_add: an example of a function that takes two complex
;;; numbers and returns another complex number
(defun complex-add (c1 c2)
  (foreign-funcall "gsl_complex_add" complex c1 complex c2 complex))

;;; gsl_complex_add_real: an example of a function that takes one complex
;;; number and one real number and returns another complex number

(defcfun (complex-add-real "gsl_complex_add_real") complex
  "Add the real number to the complex number."
  (c complex) (r :double))

;;; Definition of complex-add-real using the preparation from the
;;; defcfun above.
(defun complex-add-real-ff-prep (c r)
  (foreign-funcall complex-add-real complex c :double r complex))

;;; Definition of complex-add-real re-preparing each call.
(defun complex-add-real-ff-noprep (c r)
  (foreign-funcall "gsl_complex_add_real" complex c :double r complex))
