;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; defcfun.lisp --- Tests function definition and calling.
;;;
;;; Copyright (C) 2005, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;# Calling with built-in c types
;;;
;;; Tests calling standard C library functions both passing
;;; and returning each built-in type. (adapted from funcall.lisp)

(defcfun "toupper" :char
  (char :char))

(deftest defcfun.char
    (toupper (char-code #\a))
  #.(char-code #\A))


(defcfun ("abs" c-abs) :int
  (n :int))

(deftest defcfun.int
    (c-abs -100)
  100)


(defcfun "labs" :long
  (n :long))

(deftest defcfun.long
    (labs -131072)
  131072)


(defcfun "my_sqrtf" :float
  (n :float))

(deftest defcfun.float
    (my-sqrtf 16.0)
  4.0)


(defcfun ("sqrt" c-sqrt) :double
  (n :double))

(deftest defcfun.double
    (c-sqrt 36.0d0)
  6.0d0)


(defcfun "strlen" :int
  (n :string))

(deftest defcfun.string.1
    (strlen "Hello")
  5)


(defcfun "strcpy" :pointer
  (dest :pointer)
  (src :string))

(defcfun "strcat" :pointer
  (dest :pointer)
  (src :string))

(deftest defcfun.string.2
    (with-foreign-ptr-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (strcpy s "Hello")
      (strcat s ", world!"))
  "Hello, world!")

(defcfun "strerror" :string
  (n :int))

(deftest defcfun.string.3
    (typep (strerror 1) 'string)
  t)


;;; Regression test. Allegro would warn on direct calls to
;;; functions with no arguments.
;;; Also, let's check if void functions will return NIL.

(defcfun "noargs" :int)

(deftest defcfun.noargs
    (noargs)
  42)

(defcfun "noop" :void)

(deftest defcfun.noop
    (noop)
  nil)