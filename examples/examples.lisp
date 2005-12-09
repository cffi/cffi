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
  (with-foreign-pointer-as-string (buf 255 buf-size)
    (%gethostname buf buf-size)))

;; This definition uses the STRING type translator to automatically
;; convert Lisp strings to foreign strings and vice versa.
(defcfun "getenv" :string
  (name :string))

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
      (%gettimeofday tv (null-pointer))
      (values tv-sec tv-usec))))

;; Calling a varargs function.
(defun sprintf-test ()
  "Test calling a varargs function."
  (with-foreign-pointer-as-string (buf 255 buf-size)
    (foreign-funcall
     "snprintf" :pointer buf :int buf-size
     :string "%d %f #x%x!" :int 666 
     :double (coerce pi 'double-float)
     :unsigned-int #xcafebabe
     :void)))

;; Defining an emerated type.
(defcenum test-enum
  (:invalid 0)
  (:positive 1)
  (:negative -1))

;; Use the absolute value function to test keyword/enum translation.
(defcfun ("abs" c-abs) test-enum
  (n test-enum))

;; Nested structure example:
(defcstruct nested-example
  (name    :char :count 100)
  (timeval timeval))

(defun nested-example ()
  "Example using nested structures."
  (with-foreign-object (n nested-example)
    (foreign-slot-value n 'nested-example 'timeval)
    ;; This signals an error!
    (setf (foreign-slot-value n 'nested-example 'timeval) nil)))

#-(and)
(defcstruct nested-example2
  (times timeval :count 20))

#-(and)
(defun nested-example2 ()
  (with-foreign-object (n nested-example2)
    ;; Set the fields of each of timeval in N to zero.
    (dotimes (i 20)
      ;; n->times[i].tv_sec = 0; n->times[i].tv_usec = 0;
      (setf (foreign-slot-value n 'nested-example2 'times i 'tv-sec) 0)
      (setf (foreign-slot-value n 'nested-example2 'times i 'tv-usec) 0))
    ;; In the real world, we'd do this instead:
    (mem-fill n :uint8 (foreign-type-size 'nested-example2) 0)))

;; Access the nested structure fields with:
;;
;; (foreign-slot-value ptr 'nested-example 'timeval 'tv-sec)

(defun cffi-version ()
  (asdf:component-version (asdf:find-system 'cffi)))

(defun run-examples ()
  (format t "~&;;; CFFI version ~A on ~A ~A:~%"
          (cffi-version) (lisp-implementation-type)
          (lisp-implementation-version))
  (format t "~&;; hostname:          ~A~%" (gethostname))
  (format t "~&;; shell:             ~A~%" (getenv "SHELL"))
  (format t "~&;; sprintf test:      ~A~%" (sprintf-test))
  (format t "~&;; gettimeofday:      ~A~%" (multiple-value-list (gettimeofday)))
  (format t "~&;; (c-abs :positive): ~A~%" (c-abs :positive))
  (format t "~&;; (c-abs :negative): ~A~%" (c-abs :negative))
  (force-output))
