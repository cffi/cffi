;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; Time-stamp: <2011-08-28 22:48:54EDT structures.lisp>
;;;
;;; strings.lisp --- Operations on foreign strings.
;;;
;;; Copyright (C) 2011, Liam M. Healy  <lhealy@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi)

;;; Definitions for conversion of foreign structures.

(defmacro define-structure-conversion
    (value type slot-names to-body from-body &optional (struct-name type))
  "Define the functions necessary to convert to and from a foreign structure."
  `(progn
     (defmethod translate-to-foreign ((,value ,stuct-name) (type ,type))
       (let ((p (foreign-alloc ',struct-name)))
	 (maphash #'translate-to-foreign (slots ,struct-name)) ; recursive translation of slots
	 (iterate-slots (slot slot-type p ,type) )
	 (with-foreign-slots (,slot-names p ',struct-name)
	   ,@to-body)
	 (values p t))) ; second value is passed to FREE-TRANSLATED-OBJECT
     (defmethod free-translated-object (,value (p ,type) freep)
       (when freep
	 (maphash #'free-translated-object (slots ,struct-name)) ; recursively free slots
	 (foreign-free ,value)))
     (defmethod translate-from-foreign (,value (type ,type))
       ,@from-body)))

#| Example
(defcstruct (complex-double-c :class complex-double-c)
 (real :double)
 (imag :double))

(define-structure-conversion value complex-double-c (real imag)
  ((setf real (realpart value)
	 imag (imagpart value)))
  ((complex (foreign-slot-value value complex-double-c real)
	    (foreign-slot-value value complex-double-c imag))))
|#

