;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; struct-tests.lisp --- Test access of foreign structures.
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

;; TODO: Make a separate CFFI-TESTS package for this later.
(in-package #:cffi)

(load-foreign-library
 (namestring
  (merge-pathnames
   "struct-tests.so"
   (make-pathname :name nil :defaults *load-truename*))))

(defcstruct align-test
  (c :char)
  (d :double)
  (s :short)
  (i :int))

(defcfun "do_align_test" :void
  (a :pointer))

(defun align-test ()
  "Load an ALIGN-TEST foreign structure."
  (with-foreign-struct (a align-test)
    (do-align-test a)
    (values
     (foreign-slot-value a 'align-test 'c)
     (foreign-slot-value a 'align-test 'd)
     (foreign-slot-value a 'align-test 's)
     (foreign-slot-value a 'align-test 'i))))
