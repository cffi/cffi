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

#-cffi-features:no-long-long
(progn
  (defcfun "expect_long_long_sum"          :int (f :pointer))
  (defcfun "expect_unsigned_long_long_sum" :int (f :pointer)))


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

#-cffi-features:no-long-long
(progn
  (defcallback sum-long-long :long-long
      ((a :long-long) (b :long-long))
    (+ a b))
  
  (defcallback sum-unsigned-long-long :unsigned-long-long
      ((a :unsigned-long-long) (b :unsigned-long-long))
    (+ a b)))

(defcallback sum-float :float ((a :float) (b :float))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-double :double ((a :double) (b :double))
  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
  (+ a b))

(defcallback sum-pointer :pointer ((ptr :pointer) (offset :int))
  (inc-pointer ptr offset))

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

#-cffi-features:no-long-long
(progn
  #+openmcl (push 'callbacks.long-long rt::*expected-failures*)

  (deftest callbacks.long-long
      (expect-long-long-sum (callback sum-long-long))
    1)
  
  (deftest callbacks.unsigned-long-long
      (expect-unsigned-long-long-sum (callback sum-unsigned-long-long))
    1))

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

#-cffi-features:no-foreign-funcall
(defcallback return-a-string-not-nil :string ()
  "abc")

#-cffi-features:no-foreign-funcall
(deftest callbacks.string-not-docstring
    (foreign-funcall (callback return-a-string-not-nil) :string)
  "abc")

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

;;; void callback
(defparameter *int* -1)

(defcfun "pass_int_ref" :void (f :pointer))

(defcallback read-int-from-pointer :void ((a :pointer))
  (setq *int* (mem-ref a :int)))

(deftest callbacks.void
    (progn
      (pass-int-ref (callback read-int-from-pointer))
      *int*)
  1984)

;;; test funcalling of a callback and also declarations inside
;;; callbacks.

#-cffi-features:no-foreign-funcall
(progn
  (defcallback sum-2 :int ((a :int) (b :int) (c :int))
    (declare (ignore c))
    (+ a b))

  (deftest callbacks.funcall.1
      (foreign-funcall (callback sum-2) :int 2 :int 3 :int 1 :int)
    5)

  (defctype foo-float :float)

  (defcallback sum-2f foo-float
      ((a foo-float) (b foo-float) (c foo-float) (d foo-float) (e foo-float))
    "This one ignores the middle 3 arguments."
    (declare (ignore b c))
    (declare (ignore d))
    (+ a e))

  (deftest callbacks.funcall.2
      (foreign-funcall (callback sum-2f) foo-float 1.0 foo-float 2.0
                       foo-float 3.0 foo-float 4.0 foo-float 5.0 foo-float)
    6.0))

;;; (cb-test :no-long-long t)

(defcfun "call_sum_127_no_ll" :long (cb :pointer))
 
(defcallback sum-127-no-ll :long
    ((a1 :unsigned-long) (a2 :pointer) (a3 :long) (a4 :double)
     (a5 :unsigned-long) (a6 :float) (a7 :float) (a8 :int) (a9 :unsigned-int)
     (a10 :double) (a11 :double) (a12 :double) (a13 :pointer)
     (a14 :unsigned-short) (a15 :unsigned-short) (a16 :pointer) (a17 :long)
     (a18 :long) (a19 :int) (a20 :short) (a21 :unsigned-short)
     (a22 :unsigned-short) (a23 :char) (a24 :long) (a25 :pointer) (a26 :pointer)
     (a27 :char) (a28 :unsigned-char) (a29 :unsigned-long) (a30 :short)
     (a31 :int) (a32 :int) (a33 :unsigned-char) (a34 :short) (a35 :long)
     (a36 :long) (a37 :pointer) (a38 :unsigned-short) (a39 :char) (a40 :double)
     (a41 :unsigned-short) (a42 :pointer) (a43 :short) (a44 :unsigned-long)
     (a45 :unsigned-short) (a46 :float) (a47 :unsigned-char) (a48 :short)
     (a49 :float) (a50 :short) (a51 :char) (a52 :unsigned-long)
     (a53 :unsigned-long) (a54 :char) (a55 :float) (a56 :long) (a57 :pointer)
     (a58 :short) (a59 :float) (a60 :unsigned-int) (a61 :float)
     (a62 :unsigned-int) (a63 :double) (a64 :unsigned-int) (a65 :unsigned-char)
     (a66 :int) (a67 :long) (a68 :char) (a69 :short) (a70 :double) (a71 :int)
     (a72 :pointer) (a73 :char) (a74 :unsigned-short) (a75 :pointer)
     (a76 :unsigned-short) (a77 :pointer) (a78 :unsigned-long) (a79 :double)
     (a80 :pointer) (a81 :long) (a82 :float) (a83 :unsigned-short)
     (a84 :unsigned-short) (a85 :pointer) (a86 :float) (a87 :int)
     (a88 :unsigned-int) (a89 :double) (a90 :float) (a91 :long) (a92 :pointer)
     (a93 :unsigned-short) (a94 :float) (a95 :unsigned-char) (a96 :unsigned-char)
     (a97 :float) (a98 :unsigned-int) (a99 :float) (a100 :unsigned-short)
     (a101 :double) (a102 :unsigned-short) (a103 :unsigned-long)
     (a104 :unsigned-int) (a105 :unsigned-long) (a106 :pointer)
     (a107 :unsigned-char) (a108 :char) (a109 :char) (a110 :unsigned-short)
     (a111 :unsigned-long) (a112 :float) (a113 :short) (a114 :pointer)
     (a115 :long) (a116 :unsigned-short) (a117 :short) (a118 :double)
     (a119 :short) (a120 :int) (a121 :char) (a122 :unsigned-long) (a123 :long)
     (a124 :int) (a125 :pointer) (a126 :double) (a127 :unsigned-char))
  (+ a1 (pointer-address a2) a3 (values (floor a4)) a5 (values (floor a6))
     (values (floor a7)) a8 a9 (values (floor a10)) (values (floor a11))
     (values (floor a12)) (pointer-address a13) a14 a15 (pointer-address a16)
     a17 a18 a19 a20 a21 a22 a23 a24 (pointer-address a25)
     (pointer-address a26) a27 a28 a29 a30 a31 a32 a33 a34 a35 a36
     (pointer-address a37) a38 a39 (values (floor a40)) a41
     (pointer-address a42) a43 a44 a45 (values (floor a46)) a47 a48
     (values (floor a49)) a50 a51 a52 a53 a54 (values (floor a55))
     a56 (pointer-address a57) a58 (values (floor a59)) a60
     (values (floor a61)) a62 (values (floor a63)) a64 a65 a66 a67 a68 a69
     (values (floor a70)) a71 (pointer-address a72) a73 a74
     (pointer-address a75) a76 (pointer-address a77) a78 (values (floor a79))
     (pointer-address a80) a81 (values (floor a82)) a83 a84
     (pointer-address a85) (values (floor a86)) a87 a88 (values (floor a89))
     (values (floor a90)) a91 (pointer-address a92) a93 (values (floor a94))
     a95 a96 (values (floor a97)) a98 (values (floor a99)) a100
     (values (floor a101)) a102 a103 a104 a105 (pointer-address a106) a107
     a108 a109 a110 a111 (values (floor a112)) a113 (pointer-address a114)
     a115 a116 a117 (values (floor a118)) a119 a120 a121 a122 a123 a124
     (pointer-address a125) (values (floor a126)) a127))

(deftest callbacks.bff.1
    (call-sum-127-no-ll (callback sum-127-no-ll))
  2008547941)

;;; (cb-test)

#-cffi-features:no-long-long
(progn
  (defcfun "call_sum_127" :long-long (cb :pointer))
  
  (defcallback sum-127 :long-long
      ((a1 :short) (a2 :char) (a3 :pointer) (a4 :float) (a5 :long) (a6 :double)
       (a7 :unsigned-long-long) (a8 :unsigned-short) (a9 :unsigned-char)
       (a10 :char) (a11 :char) (a12 :unsigned-short) (a13 :unsigned-long-long)
       (a14 :unsigned-short) (a15 :long-long) (a16 :unsigned-short)
       (a17 :unsigned-long-long) (a18 :unsigned-char) (a19 :unsigned-char)
       (a20 :unsigned-long-long) (a21 :long-long) (a22 :char) (a23 :float)
       (a24 :unsigned-int) (a25 :float) (a26 :float) (a27 :unsigned-int)
       (a28 :float) (a29 :char) (a30 :unsigned-char) (a31 :long) (a32 :long-long)
       (a33 :unsigned-char) (a34 :double) (a35 :long) (a36 :double)
       (a37 :unsigned-int) (a38 :unsigned-short) (a39 :long-long)
       (a40 :unsigned-int) (a41 :int) (a42 :unsigned-long-long) (a43 :long)
       (a44 :short) (a45 :unsigned-int) (a46 :unsigned-int)
       (a47 :unsigned-long-long) (a48 :unsigned-int) (a49 :long) (a50 :pointer)
       (a51 :unsigned-char) (a52 :char) (a53 :long-long) (a54 :unsigned-short)
       (a55 :unsigned-int) (a56 :float) (a57 :unsigned-char) (a58 :unsigned-long)
       (a59 :long-long) (a60 :float) (a61 :long) (a62 :float) (a63 :int)
       (a64 :float) (a65 :unsigned-short) (a66 :unsigned-long-long) (a67 :short)
       (a68 :unsigned-long) (a69 :long) (a70 :char) (a71 :unsigned-short)
       (a72 :long-long) (a73 :short) (a74 :double) (a75 :pointer)
       (a76 :unsigned-int) (a77 :char) (a78 :unsigned-int) (a79 :pointer)
       (a80 :pointer) (a81 :unsigned-char) (a82 :pointer) (a83 :unsigned-short)
       (a84 :unsigned-char) (a85 :long) (a86 :pointer) (a87 :char) (a88 :long)
       (a89 :unsigned-short) (a90 :unsigned-char) (a91 :double)
       (a92 :unsigned-long-long) (a93 :unsigned-short) (a94 :unsigned-short)
       (a95 :unsigned-int) (a96 :long) (a97 :char) (a98 :long) (a99 :char)
       (a100 :short) (a101 :unsigned-short) (a102 :unsigned-long)
       (a103 :unsigned-long) (a104 :short) (a105 :long-long) (a106 :long-long)
       (a107 :long-long) (a108 :double) (a109 :unsigned-short)
       (a110 :unsigned-char) (a111 :short) (a112 :unsigned-char) (a113 :long)
       (a114 :long-long) (a115 :unsigned-long-long) (a116 :unsigned-int)
       (a117 :unsigned-long) (a118 :unsigned-char) (a119 :long-long)
       (a120 :unsigned-char) (a121 :unsigned-long-long) (a122 :double)
       (a123 :unsigned-char) (a124 :long-long) (a125 :unsigned-char)
       (a126 :char) (a127 :long-long))
    (+ a1 a2 (pointer-address a3) (values (floor a4)) a5 (values (floor a6))
       a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22
       (values (floor a23)) a24 (values (floor a25)) (values (floor a26))
       a27 (values (floor a28)) a29 a30 a31 a32 a33 (values (floor a34))
       a35 (values (floor a36)) a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47
       a48 a49 (pointer-address a50) a51 a52 a53 a54 a55 (values (floor a56))
       a57 a58 a59 (values (floor a60)) a61 (values (floor a62)) a63
       (values (floor a64)) a65 a66 a67 a68 a69 a70 a71 a72 a73
       (values (floor a74)) (pointer-address a75) a76 a77 a78
       (pointer-address a79) (pointer-address a80) a81 (pointer-address a82)
       a83 a84 a85 (pointer-address a86) a87 a88 a89 a90 (values (floor a91))
       a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107
       (values (floor a108)) a109 a110 a111 a112 a113 a114 a115 a116 a117 a118
       a119 a120 a121 (values (floor a122)) a123 a124 a125 a126 a127))
  
  (deftest callbacks.bff.2
      (call-sum-127 (callback sum-127))
    8166570665645582011))

;;; regression test: (callback non-existant-callback) should throw an error

(deftest callbacks.non-existant
    (not (null (nth-value 1 (ignore-errors (callback doesnt-exist)))))
  t)