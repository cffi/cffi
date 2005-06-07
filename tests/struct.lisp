;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; struct.lisp --- Foreign structure type tests.
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

(defcstruct timeval
  (tv-secs :long)
  (tv-usecs :long))

(defparameter *timeval-size* (* 2 (max (foreign-type-size :long)
                                       (foreign-type-alignment :long))))

;;;# Basic Structure Tests

(deftest struct.1
    (- (foreign-type-size 'timeval) *timeval-size*)
  0)

(deftest struct.2
    (with-foreign-object (tv timeval)
      (setf (foreign-slot-value tv 'timeval 'tv-secs) 0)
      (setf (foreign-slot-value tv 'timeval 'tv-usecs) 1)
      (values (foreign-slot-value tv 'timeval 'tv-secs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  0 1)

(deftest struct.3
    (with-foreign-object (tv timeval)
      (with-foreign-slots ((tv-secs tv-usecs) tv timeval)
        (setf tv-secs 100 tv-usecs 200)
        (values tv-secs tv-usecs)))
  100 200)
