;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; fsbv.lisp --- Tests of foreign structure by value calls.
;;;
;;; Copyright (C) 2011, Liam M. Healy
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

;; Requires struct.lisp

(defcfun "sumpair" :int
  (p (:struct struct-pair)))

(defcfun "doublepair" (:struct struct-pair)
  (p (:struct struct-pair)))

(defcfun "prodsumpair" :double
  (p (:struct struct-pair+double)))

(defcfun "doublepairdouble" (:struct struct-pair+double)
  (p (:struct struct-pair+double)))

;;; Call struct by value
(deftest fsbv.1
    (sumpair '(1 . 2))
  3)

;;; Call and return struct by value
(deftest fsbv.2
    (doublepair '(1 . 2))
  '(2 . 4))

;;; Call recursive structure by value
(deftest fsbv.3
    (prodsumpair '(pr (a 4 b 5) dbl 2.5d0))
  22.5d0)

;;; Call and return recursive structure by value
(deftest fsbv.4
    (let ((ans (doublepairdouble '(pr (a 4 b 5) dbl 2.5d0))))
      (values (getf (getf ans 'pr) 'a)
	      (getf (getf ans 'pr) 'b)
	      (getf ans 'dbl)))
  8
  10
  5.0d0)

;;; From Ryan Pavlik

(cffi:defcstruct rp-struct
 (x :short)
 (y :short))

(defun test-old-ref (&optional (count 0))
 (declare (notinline cffi:mem-ref cffi:mem-aref))
 (cffi:with-foreign-object (ptr '(:struct rp-struct) 2)
   (format t "~&Old-ref style:~%ptr : ~A~%aref: ~A~%"
           ptr (cffi:mem-aref ptr 'rp-struct count))))

(defun test-new-ref ()
 (cffi:with-foreign-object (ptr '(:struct rp-struct) 2)
   (format t "~&New-ref style:~%ptr : ~A~%aref: ~A~%"
          ptr
          (cffi:mem-aref ptr '(:struct rp-struct) 1))))

(defun test-new-ptr-ref (&optional (count 0))
 (cffi:with-foreign-object (ptr '(:struct rp-struct) 2)
   (format t "~&New-ref with :pointer style:~%ptr : ~A~%aref: ~A~%"
          ptr
          (cffi:mem-aref ptr '(:pointer (:struct rp-struct)) count))))

(defun test-generic-ptr-ref (&optional (count 0))
 (cffi:with-foreign-object (ptr '(:struct rp-struct) 2)
   (format t "~&Generic :pointer ref:~%ptr : ~A~%aref: ~A~%"
          ptr
          (cffi:mem-aref ptr ':pointer count))))
