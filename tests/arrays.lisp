;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; arrays.lisp --- Tests for foreign arrays.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;#Foreign Array Conversion Tests
;;;

(in-package #:cffi-tests)

(deftest array.foreign-to-lisp.basic
    (with-foreign-array (ptr #(1 2 3 4 5) '(:array :int32 5))
      (foreign-array-to-lisp ptr '(:array :int32 5)))
  #(1 2 3 4 5))

(deftest array.foreign-to-lisp.adjustable
    (with-foreign-array (ptr #(1 2 3 4 5) '(:array :int32 5))
      (let ((array (foreign-array-to-lisp ptr '(:array :int32 5)
                                          :adjustable t)))
        (adjustable-array-p array)))
  t)

(deftest array.foreign-to-lisp.displaced
    (let ((array (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 0))))
      (with-foreign-array (ptr #(10 20 30 40 50) '(:array :int32 5))
        (let ((displaced (foreign-array-to-lisp ptr '(:array :int32 5)
                                                :displaced-to array
                                                :displaced-index-offset 5)))
          array)))
  #(1 2 3 4 5 10 20 30 40 50))

;;; Implementation detail: 15.1.2.2 of the CL standard states that the only
;;; truly portable array specializations are for bits (bit-vectors) and
;;; characters (strings). Since char-codes are implementation-dependent, it
;;; would be tricky to write a portable test for them without generating
;;; characters at runtime. So, for a truly portable test, we are only left with
;;; bits, which are luckily numeric, and equal to (UNSIGNED-BYTE 1).
;;; This is why the below test is so terribly wasteful, spending a whole byte
;;; for a single bit - CFFI has no capabilities for dealing with single bits,
;;; and this test is only meant to check correctness of the :ELEMENT-TYPE
;;; argument to MAKE-ARRAY. In actual use cases of specialized
;;; FOREIGN-ARRAY-TO-LISP, capable implementations will be able to make
;;; specialized arrays of types that are commonly optimized for and/or
;;; representable in hardware, such as (UNSIGNED-BYTE 8) on x86 architectures.
(deftest array.foreign-to-lisp.specialized
    (with-foreign-array (ptr #(1 0 1 0 1 1 1 0) '(:array :int8 8))
      (foreign-array-to-lisp ptr '(:array :int8 8) :element-type 'bit))
  #*10101110)
