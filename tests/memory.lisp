;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; memory.lisp --- Tests for memory referencing.
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

(deftest deref.char
    (with-foreign-object (p :char)
      (setf (mem-ref p :char) -127)
      (mem-ref p :char))
  -127)

(deftest deref.unsigned-char
    (with-foreign-object (p :unsigned-char)
      (setf (mem-ref p :unsigned-char) 255)
      (mem-ref p :unsigned-char))
  255)

(deftest deref.short
    (with-foreign-object (p :short)
      (setf (mem-ref p :short) -32767)
      (mem-ref p :short))
  -32767)

(deftest deref.unsigned-short
    (with-foreign-object (p :unsigned-short)
      (setf (mem-ref p :unsigned-short) 65535)
      (mem-ref p :unsigned-short))
  65535)

(deftest deref.int
    (with-foreign-object (p :int)
      (setf (mem-ref p :int) -131072)
      (mem-ref p :int))
  -131072)

(deftest deref.unsigned-int
    (with-foreign-object (p :unsigned-int)
      (setf (mem-ref p :unsigned-int) 262144)
      (mem-ref p :unsigned-int))
  262144)

(deftest deref.long
    (with-foreign-object (p :long)
      (setf (mem-ref p :long) -536870911)
      (mem-ref p :long))
  -536870911)

(deftest deref.unsigned-long
    (with-foreign-object (p :unsigned-long)
      (setf (mem-ref p :unsigned-long) 536870912)
      (mem-ref p :unsigned-long))
  536870912)

(deftest deref.float.1
    (with-foreign-object (p :float)
      (setf (mem-ref p :float) 0.0)
      (mem-ref p :float))
  0.0)

(deftest deref.float.2
    (with-foreign-object (p :float)
      (setf (mem-ref p :float) most-positive-single-float)
      (mem-ref p :float))
  #.most-positive-single-float)

(deftest deref.float.3
    (with-foreign-object (p :float)
      (setf (mem-ref p :float) least-positive-single-float)
      (mem-ref p :float))
  #.least-positive-single-float)

(deftest deref.double.1
    (with-foreign-object (p :double)
      (setf (mem-ref p :double) 0.0d0)
      (mem-ref p :double))
  0.0d0)

(deftest deref.double.2
    (with-foreign-object (p :double)
      (setf (mem-ref p :double) most-positive-double-float)
      (mem-ref p :double))
  #.most-positive-double-float)

(deftest deref.double.3
    (with-foreign-object (p :double)
      (setf (mem-ref p :double) least-positive-double-float)
      (mem-ref p :double))
  #.least-positive-double-float)

;; make sure the lisp doesn't convert NULL to NIL
(deftest deref.pointer.null
    (with-foreign-object (p :pointer)
      (setf (mem-ref p :pointer) (null-ptr))
      (null-ptr-p (mem-ref p :pointer)))
  t)

;; regression test. lisp-string-to-foreign should handle empty strings

(deftest lisp-string-to-foreign.empty
    (with-foreign-ptr (str 2)
      (setf (mem-ref str :unsigned-char) 42)
      (lisp-string-to-foreign "" str 1)
      (mem-ref str :unsigned-char))
  0)

;; regression test. with-foreign-ptr shouldn't evaluate
;; the size argument twice.

(deftest with-foreign-ptr.evalx2
    (let* ((count 0))
      (with-foreign-ptr (x (incf count) size-var)
        (values count size-var)))
  1 1)