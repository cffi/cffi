;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; translator-test.lisp --- Testing type translators.
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

(defpackage #:cffi-translator-test
  (:use #:common-lisp #:cffi #:cffi-utils))

(in-package #:cffi-translator-test)

;;;# Verbose Pointer Translator
;;;
;;; This is a silly type translator that doesn't actually do any
;;; translating, but it prints out a debug message when the pointer is
;;; converted to/from its foreign representation.

(defctype verbose-pointer :pointer)

(defmethod translate-to-foreign (value (name (eql 'verbose-pointer)))
  (format *debug-io* "~&;; to foreign: VERBOSE-POINTER: ~S~%" value)
  value)

(defmethod translate-from-foreign (value (name (eql 'verbose-pointer)))
  (format *debug-io* "~&;; from foreign: VERBOSE-POINTER: ~S~%" value)
  value)

;;;# Verbose String Translator
;;;
;;; A VERBOSE-STRING is a typedef for a VERBOSE-POINTER except the
;;; Lisp string is first converted to a C string.  If things are
;;; working properly, both type translators should be called when
;;; converting a Lisp string to/from a C string.
;;;
;;; The translators should be called most-specific-first when
;;; translating to C, and most-specific-last when translating from C.

(defctype verbose-string verbose-pointer)

(defmethod translate-to-foreign ((s string) (name (eql 'verbose-string)))
  (let ((value (foreign-string-alloc s)))
    (format *debug-io* "~&;; to foreign: VERBOSE-STRING: ~S -> ~S~%" s value)
    (values value t)))

(defmethod translate-to-foreign (value (name (eql 'verbose-string)))
  (if (pointerp value)
      (progn
        (format *debug-io* "~&;; to foreign: VERBOSE-STRING: ~S -> ~:*~S~%" value)
        (values value nil))
      (error "Cannot convert ~S to a foreign string: it is not a Lisp ~
              string or pointer." value)))

(defmethod translate-from-foreign (ptr (name (eql 'verbose-string)))
  (let ((value (foreign-string-to-lisp ptr)))
    (format *debug-io* "~&;; from foreign: VERBOSE-STRING: ~S -> ~S~%" ptr value)
    value))

(defmethod free-translated-object (ptr (name (eql 'verbose-string)) free-p)
  (when free-p
    (foreign-string-free ptr)))

(defun test-verbose-string ()
  (foreign-funcall "getenv" verbose-string "SHELL" verbose-string))

;;;# Testing Chained Parameters

(defctype inner-type :int)
(defctype middle-type inner-type)
(defctype outer-type middle-type)

(defmethod translate-to-foreign (value (name (eql 'inner-type)))
  (values value 1))

(defmethod translate-to-foreign (value (name (eql 'middle-type)))
  (values value 2))

(defmethod translate-to-foreign (value (name (eql 'outer-type)))
  (values value 3))

(defmethod free-translated-object (value (name (eql 'inner-type)) param)
  (format t "~&;; free inner-type ~A~%" param))

(defmethod free-translated-object (value (name (eql 'middle-type)) param)
  (format t "~&;; free middle-type ~A~%" param))

(defmethod free-translated-object (value (name (eql 'outer-type)) param)
  (format t "~&;; free outer-type ~A~%" param))
