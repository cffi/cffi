;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; funcall.lisp --- Tests function calling.
;;;
;;; Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
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

(in-package #:cffi-tests)

;;;# Calling with Built-In C Types
;;;
;;; Tests calling standard C library functions both passing and
;;; returning each built-in type.

;;; Don't run these tests if the implementation does not support
;;; foreign-funcall.
#-cffi-features:no-foreign-funcall 
(progn

(deftest funcall.char
    (foreign-funcall "toupper" :char (char-code #\a) :char)
  #.(char-code #\A))

(deftest funcall.int.1
    (foreign-funcall "abs" :int -100 :int)
  100)

(defun funcall-abs (n)
  (foreign-funcall "abs" :int n :int))

;;; regression test: lispworks's %foreign-funcall based on creating
;;; and chaching foreign-funcallables at macro-expansion time.
(deftest funcall.int.2
    (funcall-abs -42)
  42)

(deftest funcall.long
    (foreign-funcall "labs" :long -131072 :long)
  131072)

#-cffi-features:no-long-long
(deftest funcall.long-long
    (foreign-funcall "llabs" :long-long -9223372036854775807 :long-long)
  9223372036854775807)

(deftest funcall.float
    (foreign-funcall "my_sqrtf" :float 16.0 :float)
  4.0)

(deftest funcall.double
    (foreign-funcall "sqrt" :double 36.0d0 :double)
  6.0d0)

(deftest funcall.string.1
    (foreign-funcall "strlen" :string "Hello" :int)
  5)

(deftest funcall.string.2
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "strcpy" :pointer s :string "Hello" :pointer)
      (foreign-funcall "strcat" :pointer s :string ", world!" :pointer))
  "Hello, world!")

(deftest funcall.string.3
    (with-foreign-pointer (ptr 100)
      (lisp-string-to-foreign "Hello, " ptr 8)
      (foreign-funcall "strcat" :pointer ptr :string "world!" :string))
  "Hello, world!")

;;;# Calling Varargs Functions

;; The CHAR argument must be passed as :INT because chars are promoted
;; to ints when passed as variable arguments.
(deftest funcall.varargs.char
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%c" :int 65 :int))
  "A")

(deftest funcall.varargs.int
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%d" :int 1000 :int))
  "1000")

(deftest funcall.varargs.long
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%ld" :long 131072 :int))
  "131072")

;;; There is no FUNCALL.VARARGS.FLOAT as floats are promoted to double
;;; when passed as variable arguments.  Currently this fails in SBCL
;;; and CMU CL on Darwin/ppc.
(deftest funcall.varargs.double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%.2f"
                       :double (coerce pi 'double-float) :int))
  "3.14")

(deftest funcall.varargs.string
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%s, %s!"
                       :string "Hello" :string "world" :int))
  "Hello, world!")

) ;; #-cffi-features:no-foreign-funcall
