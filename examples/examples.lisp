;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; examples.lisp --- Simple test examples of CFFI.
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

(defpackage #:cffi-examples
  (:use #:cl #:cffi)
  (:export
   #:run-examples
   #:sqrtf
   #:gethostname
   #:getenv
   #:gettimeofday))

(in-package #:cffi-examples)

;; A simple libc function.
(defcfun "sqrtf" :float
  (n :float))

;; Due to the awkward calling convention of 'gethostname' we first
;; define an internal function using DEFCFUN.
(defcfun ("gethostname" %gethostname) :int
  (buf :pointer)
  (bufsize :int))

;; Now we define a Lispy interface to %GETHOSTNAME.
(defun gethostname ()
  "Return the hostname using the 'gethostname' call."
  (with-foreign-ptr-as-string (buf 255 buf-size)
    (%gethostname buf buf-size)))

;; This definition uses the STRING type translator to automatically
;; convert Lisp strings to foreign strings and vice versa.
(defcfun "getenv" string
  (name string))

;; Defining a foreign structure.
(defcstruct timeval
  (tv-sec  :long)
  (tv-usec :long))

;; Define an interface to 'gettimeofday'.  The caller must allocate
;; space for the TIMEVAL structure.  TZP is deprecated and should be
;; null.
(declaim (inline %gettimeofday))
(defcfun ("gettimeofday" %gettimeofday) :int
  (tp  :pointer)
  (tzp :pointer))

;; Here is the Lispy interface to %GETTIMEOFDAY.  It returns the two
;; fields of the TIMEVAL as multiple values.
(defun gettimeofday ()
  "Return the time in seconds and microseconds."
  (with-foreign-object (tv timeval)
    (with-foreign-slots ((tv-sec tv-usec) tv timeval)
      (%gettimeofday tv (null-ptr))
      (values tv-sec tv-usec))))

;; Calling a varargs function.
(defun sprintf-test ()
  "Test calling a varargs function."
  (with-foreign-ptr-as-string (buf 255 buf-size)
    (foreign-funcall
     "snprintf" :pointer buf :int buf-size
     string "%d #x%x!" :int 666 :unsigned-int #xcafebabe
     :void)))

;; Proposed syntax for nested structures:
#+nil
(defcstruct nested-example
  (name    :pointer)
  (timeval :struct timeval))

;; Access the nested structure fields with:
;;
;; (foreign-slot-value ptr 'nested-example 'timeval 'tv-sec)

(defun cffi-version ()
  (asdf:component-version (asdf:find-system 'cffi)))

(defun run-examples ()
  (format t "~&;;; CFFI version ~A on ~A ~A:~%"
          (cffi-version) (lisp-implementation-type)
          (lisp-implementation-version))
  (format t "~&;; hostname:     ~A~%" (gethostname))
  (format t "~&;; shell:        ~A~%" (getenv "SHELL"))
  (format t "~&;; sprintf test: ~A~%" (sprintf-test))
  (format t "~&;; gettimeofday: ~A~%" (multiple-value-list (gettimeofday)))
  (force-output))

