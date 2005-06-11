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
      (setf (mem-ref p :char) 127)
      (mem-ref p :char))
  127)

(deftest deref.short
    (with-foreign-object (p :short)
      (setf (mem-ref p :short) 32767)
      (mem-ref p :short))
  32767)

(deftest deref.int
    (with-foreign-object (p :int)
      (setf (mem-ref p :int) 131072)
      (mem-ref p :int))
  131072)

(deftest deref.long
    (with-foreign-object (p :long)
      (setf (mem-ref p :long) 536870912)
      (mem-ref p :long))
  536870912)
