;; Examples of using CFFI-FSBV
;; Liam Healy 2009-04-07 22:13:34EDT examples.lisp
;; Time-stamp: <2011-09-28 22:42:01EDT examples.lisp>

(in-package :cffi)			; cffi-test ?  doesn't load

;;; These examples are based on GSL functions using and returning complex numbers
;;; http://www.gnu.org/software/gsl/manual/html_node/Properties-of-complex-numbers.html
;;; http://www.gnu.org/software/gsl/manual/html_node/Complex-arithmetic-operators.html

;;; Load the libraries
(cffi:load-foreign-library #+unix "libgslcblas.so")
(cffi:load-foreign-library #+unix "libgsl.so")

;;; Define the foreign struct; see /usr/include/gsl/gsl_complex.h
(defcstruct (complex-double :class complex-type)
  (dat :double :count 2))

(defmethod translate-into-foreign-memory ((value complex) (type complex-type) p)
  (with-foreign-slots ((dat) p (:struct complex-double))
    (setf (cffi:mem-aref dat :double 0) (realpart value)
	  (cffi:mem-aref dat :double 1) (imagpart value))))

(defmethod translate-from-foreign (p (type complex-type))
  (with-foreign-slots ((dat) p (:struct complex-double))
    (complex (cffi:mem-aref dat :double 0)
	     (cffi:mem-aref dat :double 1))))

(defmethod free-translated-object (value (p complex-type) freep)
  (declare (ignore freep))
  (foreign-free value))

(foreign-funcall "gsl_complex_conjugate"
		 (:struct complex-double) #C(3.0d0 4.0d0) (:struct complex-double))
(foreign-funcall "gsl_complex_abs" (:struct complex-double) #C(3.0d0 4.0d0) :double)

;;; gsl_complex_abs: an example of a function that takes a complex
;;; number and returns a double-float
(defcfun (complex-abs "gsl_complex_abs") :double
  (complex-number (:struct cffi::complex-double)))

;;; gsl_complex_conjugate: an example of a function that takes a complex
;;; number and returns another complex number
(defcfun (complex-conjugate "gsl_complex_conjugate") (:struct cffi::complex-double)
  "Find the complex conjugate of the given complex number."
  (c (:struct cffi::complex-double)))

(defcstruct (real-and-complex :class real-and-complex-type)
 (r :double)
 (c (:struct complex-double)))

;;; (convert-to-foreign '(r 7.0d0 c #C(2.0d0 3.0d0)) '(:struct real-and-complex))


#|

;;; Real-and-complex:


(defmethod translate-into-foreign-memory (value (type real-and-complex-type) p)
  (setf (foreign-slot-value p 'real-and-complex 'r) (first value))
  (convert-into-foreign-memory
   (second value)
   'complex
   (foreign-slot-pointer p 'real-and-complex 'c)))
(defmethod translate-from-foreign (p (type real-and-complex-type))
 (with-foreign-slots ((r c) p real-and-complex)
   (list r c)))
(convert-to-foreign '(7.0d0 #C(2.0d0 3.0d0)) 'real-and-complex)
#.(SB-SYS:INT-SAP #X0063D450)
(convert-from-foreign * 'real-and-complex)
(7.0d0 #C(2.0d0 3.0d0))

|#

;;;;;;;; Not yet checked:

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
