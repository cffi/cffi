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

;;;# Structure Alignment Tests

(defcstruct s-ch
  (a-char :char))

(defcstruct s-s-ch
  (another-char :char)
  (a-s-ch s-ch))

(defcvar "the_s_s_ch" s-s-ch)

(deftest struct.alignment.1
    (list 'a-char (foreign-slot-value
                   (foreign-slot-value *the-s-s-ch* 's-s-ch 'a-s-ch)
                   's-ch 'a-char)
          'another-char (foreign-slot-value *the-s-s-ch* 's-s-ch 'another-char))
  (a-char 1 another-char 2))


(defcstruct s-short
  (a-char :char)
  (another-char :char)
  (a-short :short))

(defcstruct s-s-short
  (yet-another-char :char)
  (a-s-short s-short))

(defcvar "the_s_s_short" s-s-short)

(deftest struct.alignment.2
    (with-foreign-slots ((yet-another-char a-s-short) *the-s-s-short* s-s-short)
      (with-foreign-slots ((a-char another-char a-short) a-s-short s-short)
        (list 'a-char           a-char
              'another-char     another-char
              'a-short          a-short
              'yet-another-char yet-another-char)))
  (a-char 1 another-char 2 a-short 3 yet-another-char 4))


(defcstruct s-double
  (a-char :char)
  (a-double :double)
  (another-char :char))

(defcstruct s-s-double
  (yet-another-char :char)
  (a-s-double s-double)
  (a-short :short))

(defcvar "the_s_s_double" s-s-double)

(deftest struct.alignment.3
    (with-foreign-slots
        ((yet-another-char a-s-double a-short) *the-s-s-double* s-s-double)
      (with-foreign-slots ((a-char a-double another-char) a-s-double s-double)
        (list 'a-char            a-char
              'a-double          a-double
              'another-char      another-char
              'yet-another-char  yet-another-char
              'a-short           a-short)))
  (a-char 1 a-double 2.0d0 another-char 3 yet-another-char 4 a-short 5))


(defcstruct s-s-s-double
  (another-short :short)
  (a-s-s-double s-s-double)
  (last-char :char))

(defcvar "the_s_s_s_double" s-s-s-double)

(deftest struct.alignment.4
    (with-foreign-slots
        ((another-short a-s-s-double last-char) *the-s-s-s-double* s-s-s-double)
      (with-foreign-slots
          ((yet-another-char a-s-double a-short) a-s-s-double s-s-double)
        (with-foreign-slots ((a-char a-double another-char) a-s-double s-double)
          (list 'a-char            a-char
                'a-double          a-double
                'another-char      another-char
                'yet-another-char  yet-another-char
                'a-short           a-short
                'another-short     another-short
                'last-char         last-char))))
  (a-char 1 a-double 2.0d0 another-char 3 yet-another-char 4 a-short 5
   another-short 6 last-char 7))


(defcstruct empty-struct)

(defcstruct with-empty-struct
  (foo empty-struct)
  (an-int :int))

;; commented out this test because an empty struct is not valid/standard C
;; left the struct declarations anyway because they should be handled
;; gracefuly anyway.

; (defcvar "the_with_empty_struct" with-empty-struct)
;
; (deftest struct.alignment.5
;     (with-foreign-slots ((foo an-int) *the-with-empty-struct* with-empty-struct)
;       an-int)
;   42)
