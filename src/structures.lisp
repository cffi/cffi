;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; Time-stamp: <2011-09-07 23:28:51EDT structures.lisp>
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
    (value-symbol type lisp-class slot-names to-form from-form &optional (struct-name type))
  "Define the functions necessary to convert to and from a foreign structure.  The to-form sets each of the foreign slots in succession, assume the foreign object exists.  The from-form creates the Lisp object, making it with the correct value by reference to foreign slots."
  `(flet ((map-slots (fn val)
            (maphash
             (lambda (name slot-struct)
               (funcall fn (foreign-slot-value val ',type name) (slot-type slot-struct)))
             (slots (follow-typedefs (parse-type ',type))))))
     ;; Convert this to a separate function so it doesn't have to be recomputed on the fly each time.
     (defmethod translate-to-foreign ((,value-symbol ,lisp-class) (type ,type))
       (let ((p (foreign-alloc ',struct-name)))
         ;;(map-slots #'translate-to-foreign ,value-symbol) ; recursive translation of slots
         (with-foreign-slots (,slot-names p ,struct-name)
           ,to-form)
         (values p t))) ; second value is passed to FREE-TRANSLATED-OBJECT
     (defmethod free-translated-object (,value-symbol (p ,type) freep)
       (when freep
         ;; Is this redundant?
         (map-slots #'free-translated-object value) ; recursively free slots
         (foreign-free ,value-symbol)))
     (defmethod translate-from-foreign (,value-symbol (type ,type))
       (with-foreign-slots (,slot-names ,value-symbol ,struct-name)
         ,from-form))))

#| Example
(defcstruct (complex-double-c :class complex-double-c)
 (real :double)
 (imag :double))

(define-structure-conversion value complex-double-c complex (real imag)
  (setf real (realpart value) imag (imagpart value))
  (complex real imag))

CFFI> (convert-to-foreign #c(3.0d0 4.0d0) 'complex-double-c)
#.(SB-SYS:INT-SAP #X006678E0)
T
CFFI> (convert-from-foreign * 'complex-double-c)
#C(3.0d0 4.0d0)

;;; Test recursive conversion:
(defcstruct (real-and-complex :class real-and-complex)
 (x :double)
 (c complex-double-c))

(define-structure-conversion value real-and-complex list (x c)
  ;; Make foreign
  ;;(setf x (first value) c (convert-to-foreign (second value) 'complex-double-c))
  (setf x (first value) c (second value))
  ;; Make CL
  (list x c))

(convert-to-foreign '(5.0d0 #c(3.0d0 4.0d0)) 'real-and-complex)

CFFI> (defparameter rac-ptr (FOREIGN-ALLOC 'REAL-AND-COMPLEX))
RAC-PTR
CFFI> rac-ptr
#.(SB-SYS:INT-SAP #X006679C0)
CFFI> (foreign-slot-value rac-ptr 'real-and-complex 'x)
0.0d0
CFFI> (foreign-slot-value rac-ptr 'real-and-complex 'c)
#C(3.2345726853444733d-317 6.9531942826387884d-310)
CFFI> rac-ptr
#.(SB-SYS:INT-SAP #X006679C0)
CFFI> (setf (foreign-slot-value rac-ptr 'real-and-complex 'x) 33.0d0)
33.0d0
CFFI> (foreign-slot-value rac-ptr 'real-and-complex 'x)
33.0d0
CFFI> (setf (foreign-slot-value rac-ptr 'real-and-complex 'c) #c(2.0d0 -8.0d0))
; Evaluation aborted on #<SIMPLE-ERROR "~@<There is no applicable method for the generic function ~2I~_~S~ .. {10060A92E1}>.
CFFI> (foreign-slot-value rac-ptr 'real-and-complex 'c)
#C(3.2345726853444733d-317 6.9531942826387884d-310)
CFFI> (foreign-slot-pointer rac-ptr 'real-and-complex 'c)
#.(SB-SYS:INT-SAP #X006679C8)
;;; But the pointer and foreign structure already exists, I can't use translate-to-foreign here.
|#


#|

;;;;****************************************************************************
;;; Luis' approach
;;;;****************************************************************************

;;; Hold off on the macros and do it all by hand at the start.
;;; There are two new functions needed:
;;; translate-into-foreign-memory
;;; convert-into-foreign-memory
;;; I assume the latter can be a regular function and bear the same relationship to translate-into-foreign-memory as convert-to-foreign does to translate-to-foreign.

(defgeneric translate-into-foreign-memory (value type p)
  (:documentation
   "Translate the Lisp value into the foreign type, writing the answers at the pointer p."))

(defun convert-into-foreign-memory (value type ptr)
  (translate-into-foreign-memory value (parse-type type) ptr)
  ptr)

;;; In order to avoid overwriting the existing T method for t-t-f, we define these specifically for foreign structures.
(defmethod translate-to-foreign (value (type foreign-struct-type))
  (let ((ptr (foreign-alloc type)))
    (translate-into-foreign-memory value type ptr)
    ptr))

;;; Complex
(defcstruct (complex :class complex-type)
 (real :double)
 (imag :double))

(defmethod translate-into-foreign-memory ((value complex) (type complex-type) p)
 (with-foreign-slots ((real imag) p complex)
   (setf real (realpart value)
         imag (imagpart value))))

(defmethod translate-from-foreign (p (type complex-type))
 (with-foreign-slots ((real imag) p complex)
   (complex real imag)))

;;; What about free-translated-object?

;;; Test of recursive conversion: real and complex
(defcstruct (real-and-complex :class real-and-complex-type)
 (x :double)
 (c complex))

(defmethod translate-into-foreign-memory (value (type real-and-complex-type) p)
  (setf (foreign-slot-value p 'real-and-complex 'x) (first value))
  (convert-into-foreign-memory
   (second value)
   'complex
   (foreign-slot-pointer p 'real-and-complex 'c)))

;;; Luis' form; this does not work, it doesn't translate the complex
(defmethod translate-from-foreign (p (type real-and-complex-type))
 (with-foreign-slots ((x c) p real-and-complex)
   (list x c)))

;;; My form; this does not work, it doesn't translate the complex
(defmethod translate-from-foreign (p (type real-and-complex-type))
  (declare (ignore type))
  (list
   (foreign-slot-value p 'real-and-complex 'x)
   (foreign-slot-value p 'real-and-complex 'c)))

;;; Plain function; this does not work, it doesn't translate the complex
(defun tff-fn (p)
  (list
   (foreign-slot-value p 'real-and-complex 'x)
   (foreign-slot-value p 'real-and-complex 'c)))

;;; This works
(defmacro tff-macro (p)
  `(list
    (foreign-slot-value ,p 'real-and-complex 'x)
    (foreign-slot-value ,p 'real-and-complex 'c)))

;;; ????

CFFI> (convert-to-foreign #C(2.0d0 3.0d0) 'complex)
#.(SB-SYS:INT-SAP #X0063E060)
CFFI> (convert-from-foreign * 'complex)
#C(2.0d0 3.0d0)
CFFI> (defparameter rac-foreign (convert-to-foreign '(1.0d0 #C(2.0d0 3.0d0)) 'real-and-complex))
;;; So far, so good, but converting back to Lisp is hard.

(convert-from-foreign rac-foreign 'real-and-complex) ; no
(tff-fn rac-foreign) ; no
(tff-macro rac-foreign) ;yes

(defun tff1 () ; no
   (foreign-slot-value rac-foreign 'real-and-complex 'c))

(defun tff2 ()                          ; yes
  (foreign-struct-slot-value rac-foreign (get-slot-info 'real-and-complex 'c)))

;;; macroexpansion of tff1
(defun tff3 ()                          ; no
  (CONVERT-FROM-FOREIGN (INC-POINTER RAC-FOREIGN 8) 'COMPLEX))

;;; macro expansion of tff3 body
;;;(INC-POINTER RAC-FOREIGN 8)

;;   0: (EXPAND-FROM-FOREIGN (INC-POINTER RAC-FOREIGN 8) #<COMPLEX-TYPE COMPLEX>)
;;   0: EXPAND-FROM-FOREIGN returned (INC-POINTER RAC-FOREIGN 8)

CFFI> (class-of (parse-type 'complex))
#<STANDARD-CLASS COMPLEX-TYPE>

;;; The problem is that expand-from-foreign just returns the first argument because complex-type is not of class enhanced-foreign-type or enhanced-typedef.

;; (define-compiler-macro convert-from-foreign (value type)

|#

