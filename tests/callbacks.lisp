;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; callbacks.lisp --- Tests on callbacks.
;;;
;;; Copyright (C) 2005, Luis Oliveira  <loliveira(@)common-lisp.net>
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

(defcfun "expect_char_sum"           :int (f :pointer))
(defcfun "expect_unsigned_char_sum"  :int (f :pointer))
(defcfun "expect_short_sum"          :int (f :pointer))
(defcfun "expect_unsigned_short_sum" :int (f :pointer))
(defcfun "expect_int_sum"            :int (f :pointer))
(defcfun "expect_unsigned_int_sum"   :int (f :pointer))
(defcfun "expect_long_sum"           :int (f :pointer))
(defcfun "expect_unsigned_long_sum"  :int (f :pointer))
(defcfun "expect_float_sum"          :int (f :pointer))
(defcfun "expect_double_sum"         :int (f :pointer))
(defcfun "expect_pointer_sum"        :int (f :pointer))
(defcfun "expect_strcat"             :int (f :pointer))


(defcallback sum-char :char ((a :char) (b :char))
  "Test if the named block is present and the docstring too."
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (return-from sum-char (+ a b)))

(defcallback sum-unsigned-char :unsigned-char
    ((a :unsigned-char) (b :unsigned-char))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-short :short ((a :short) (b :short))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-unsigned-short :unsigned-short
    ((a :unsigned-short) (b :unsigned-short))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-int :int ((a :int) (b :int))
  (+ a b))

(defcallback sum-unsigned-int :unsigned-int
    ((a :unsigned-int) (b :unsigned-int))
  (+ a b))

(defcallback sum-long :long ((a :long) (b :long))
  (+ a b))

(defcallback sum-unsigned-long :unsigned-long
    ((a :unsigned-long) (b :unsigned-long))
  (+ a b))

(defcallback sum-float :float ((a :float) (b :float))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-double :double ((a :double) (b :double))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-pointer :pointer ((ptr :pointer) (offset :int))
  (inc-ptr ptr offset))

(defcallback lisp-strcat :string ((a :string) (b :string))
  (concatenate 'string a b))


(deftest callbacks.char
    (expect-char-sum (get-callback 'sum-char))
  1)

(deftest callbacks.unsigned-char
    (expect-unsigned-char-sum (get-callback 'sum-unsigned-char))
  1)

(deftest callbacks.short
    (expect-short-sum (callback sum-short))
  1)

(deftest callbacks.unsigned-short
    (expect-unsigned-short-sum (callback sum-unsigned-short))
  1)

(deftest callbacks.int
    (expect-int-sum (callback sum-int))
  1)

(deftest callbacks.unsigned-int
    (expect-unsigned-int-sum (callback sum-unsigned-int))
  1)

(deftest callbacks.long
    (expect-long-sum (callback sum-long))
  1)

(deftest callbacks.unsigned-long
    (expect-unsigned-long-sum (callback sum-unsigned-long))
  1)

(deftest callbacks.float
    (expect-float-sum (callback sum-float))
  1)

(deftest callbacks.double
    (expect-double-sum (callback sum-double))
  1)

(deftest callbacks.pointer
    (expect-pointer-sum (callback sum-pointer))
  1)

(deftest callbacks.string
    (expect-strcat (callback lisp-strcat))
  1)

;;; This one tests mem-aref too.
(defcfun "qsort" :void
  (base :pointer)
  (nmemb :int)
  (size :int)
  (fun-compar :pointer))

(defcallback < :int ((a :pointer) (b :pointer))
  (let ((x (mem-ref a :int))
        (y (mem-ref b :int)))
    (cond ((> x y) 1)
          ((< x y) -1)
          (t 0))))

(deftest callbacks.qsort
    (with-foreign-object (array :int 10)
      ;; Initialize array.
      (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
            do (setf (mem-aref array :int i) n))
      ;; Sort it.
      (qsort array 10 (foreign-type-size :int) (callback <))
      ;; Return it as a list.
      (loop for i from 0 below 10
            collect (mem-aref array :int i)))
  (1 2 3 4 5 6 7 8 9 10))