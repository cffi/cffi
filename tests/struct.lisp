;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; struct.lisp --- Foreign structure type tests.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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
    (with-foreign-object (tv 'timeval)
      (setf (foreign-slot-value tv 'timeval 'tv-secs) 0)
      (setf (foreign-slot-value tv 'timeval 'tv-usecs) 1)
      (values (foreign-slot-value tv 'timeval 'tv-secs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  0 1)

(deftest struct.3
    (with-foreign-object (tv 'timeval)
      (with-foreign-slots ((tv-secs tv-usecs) tv timeval)
        (setf tv-secs 100 tv-usecs 200)
        (values tv-secs tv-usecs)))
  100 200)

;; regression test: accessing a struct through a typedef

(defctype xpto timeval)

(deftest struct.4
    (with-foreign-object (tv 'xpto)
      (setf (foreign-slot-value tv 'xpto 'tv-usecs) 1)
      (values (foreign-slot-value tv 'xpto 'tv-usecs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  1 1)

(deftest struct.names
    (sort (foreign-slot-names 'xpto) #'<
          :key (lambda (x) (foreign-slot-offset 'xpto x)))
  (tv-secs tv-usecs))

;; regression test: compiler macro not quoting the type in the
;; resulting mem-ref form. The compiler macro on foreign-slot-value
;; is not guaranteed to be expanded though.

(defctype my-int :int)
(defcstruct s5 (a my-int))

(deftest struct.5
    (with-foreign-object (s 's5)
      (setf (foreign-slot-value s 's5 'a) 42)
      (foreign-slot-value s 's5 'a))
  42)

;;;# Structs with type translators

(defcstruct struct-string
  (s :string))

(deftest struct.string.1
    (with-foreign-object (ptr 'struct-string)
      (with-foreign-slots ((s) ptr struct-string)
        (setf s "So long and thanks for all the fish!")
        s))
  "So long and thanks for all the fish!")

(deftest struct.string.2
    (with-foreign-object (ptr 'struct-string)
      (setf (foreign-slot-value ptr 'struct-string 's) "Cha")
      (foreign-slot-value ptr 'struct-string 's))
  "Cha")

;;;# Structure Alignment Tests
;;;
;;; See libtest.c and types.lisp for some comments about alignments.

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


(defcstruct s-double2
  (a-double :double)
  (a-short  :short))

(defcstruct s-s-double2
  (a-char        :char)
  (a-s-double2   s-double2)
  (another-short :short))

(defcvar "the_s_s_double2" s-s-double2)

(deftest struct.alignment.5
    (with-foreign-slots
        ((a-char a-s-double2 another-short) *the-s-s-double2* s-s-double2)
      (with-foreign-slots ((a-double a-short) a-s-double2 s-double2)
        (list 'a-double       a-double
              'a-short        a-short
              'a-char         a-char
              'another-short  another-short)))
  (a-double 1.0d0 a-short 2 a-char 3 another-short 4))

(defcstruct s-long-long
  (a-long-long :long-long)
  (a-short     :short))

(defcstruct s-s-long-long
  (a-char        :char)
  (a-s-long-long s-long-long)
  (another-short :short))

(defcvar "the_s_s_long_long" s-s-long-long)

(deftest struct.alignment.6
    (with-foreign-slots
        ((a-char a-s-long-long another-short) *the-s-s-long-long* s-s-long-long)
      (with-foreign-slots ((a-long-long a-short) a-s-long-long s-long-long)
        (list 'a-long-long    a-long-long
              'a-short        a-short
              'a-char         a-char
              'another-short  another-short)))
  (a-long-long 1 a-short 2 a-char 3 another-short 4))

(defcstruct s-s-double3
  (a-s-double2   s-double2)
  (another-short :short))

(defcstruct s-s-s-double3
  (a-s-s-double3  s-s-double3)
  (a-char         :char))

(defcvar "the_s_s_s_double3" s-s-s-double3)

(deftest struct.alignment.7
    (with-foreign-slots ((a-s-s-double3 a-char) *the-s-s-s-double3* s-s-s-double3)
      (with-foreign-slots ((a-s-double2 another-short) a-s-s-double3 s-s-double3)
        (with-foreign-slots ((a-double a-short) a-s-double2 s-double2)
          (list 'a-double      a-double
                'a-short       a-short
                'another-short another-short
                'a-char        a-char))))
  (a-double 1.0d0 a-short 2 another-short 3 a-char 4))


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


;; regression test, setf-ing nested foreign-slot-value forms
;; the setf expander used to return a bogus getter

(defcstruct s1
  (an-int :int))

(defcstruct s2
  (an-s1 s1))

(deftest struct.nested-setf
    (with-foreign-object (an-s2 's2)
      (setf (foreign-slot-value (foreign-slot-value an-s2 's2 'an-s1)
                                's1 'an-int)
            1984)
      (foreign-slot-value (foreign-slot-value an-s2 's2 'an-s1)
                          's1 'an-int))
  1984)

;; regression test, some Lisps were returning 4 instead of 8 for
;; (foreign-type-alignment :unsigned-long-long) on darwin/ppc32

(defcstruct s-unsigned-long-long
  (an-unsigned-long-long :unsigned-long-long)
  (a-short               :short))

(defcstruct s-s-unsigned-long-long
  (a-char                 :char)
  (a-s-unsigned-long-long s-unsigned-long-long)
  (another-short          :short))

(defcvar "the_s_s_unsigned_long_long" s-s-unsigned-long-long)

(deftest struct.alignment.8
    (with-foreign-slots
        ((a-char a-s-unsigned-long-long another-short)
         *the-s-s-unsigned-long-long* s-s-unsigned-long-long)
      (with-foreign-slots ((an-unsigned-long-long a-short)
                           a-s-unsigned-long-long s-unsigned-long-long)
        (list 'an-unsigned-long-long  an-unsigned-long-long
              'a-short                a-short
              'a-char                 a-char
              'another-short          another-short)))
  (an-unsigned-long-long 1 a-short 2 a-char 3 another-short 4))

;;;# C Struct Wrappers

(define-c-struct-wrapper timeval ())

(define-c-struct-wrapper (timeval2 timeval) ()
  (tv-secs))

(defmacro with-example-timeval (var &body body)
  `(with-foreign-object (,var 'timeval)
     (with-foreign-slots ((tv-secs tv-usecs) ,var timeval)
       (setf tv-secs 42 tv-usecs 1984)
       ,@body)))

(deftest struct-wrapper.1
    (with-example-timeval ptr
      (let ((obj (make-instance 'timeval :pointer ptr)))
        (values (timeval-tv-secs obj)
                (timeval-tv-usecs obj))))
  42 1984)

(deftest struct-wrapper.2
    (with-example-timeval ptr
      (let ((obj (make-instance 'timeval2 :pointer ptr)))
        (timeval2-tv-secs obj)))
  42)
