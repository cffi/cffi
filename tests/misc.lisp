;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; misc.lisp --- Miscellaneous tests.
;;;
;;; Copyright (C) 2006, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;# foreign-symbol-pointer tests

;;; This might be useful for some libraries that compare function
;;; pointers. http://thread.gmane.org/gmane.lisp.cffi.devel/694
(defcfun "compare_against_abs" :boolean (p :pointer))

(deftest foreign-symbol-pointer.1
    (compare-against-abs (foreign-symbol-pointer "abs"))
  t)

(defcfun "compare_against_xpto_fun" :boolean (p :pointer))

(deftest foreign-symbol-pointer.2
    (compare-against-xpto-fun (foreign-symbol-pointer "xpto_fun"))
  t)

;;;# Library tests
;;;
;;; Need to figure out a way to test this.  CLISP, for instance, will
;;; automatically reopen the foreign-library when we call a foreign
;;; function so we can't test CLOSE-FOREIGN-LIBRARY this way.
;;;
;;; IIRC, GCC has some extensions to have code run when a library is
;;; loaded and stuff like that.  That could work.

#||
#-(and ecl (not dffi))
(deftest library.close.2
    (unwind-protect
         (progn
           (close-foreign-library 'libtest)
           (ignore-errors (my-sqrtf 16.0)))
      (load-test-libraries))
  nil)

#-(or (and ecl (not dffi))
      cffi-sys::flat-namespace
      cffi-sys::no-foreign-funcall)
(deftest library.close.2
    (unwind-protect
         (values
          (foreign-funcall ("ns_function" :library libtest) :boolean)
          (close-foreign-library 'libtest)
          (foreign-funcall "ns_function" :boolean)
          (close-foreign-library 'libtest2)
          (close-foreign-library 'libtest2)
          (ignore-errors (foreign-funcall "ns_function" :boolean)))
      (load-test-libraries))
  t t nil t nil nil)
||#

(deftest library.error.1
    (handler-case (load-foreign-library "libdoesnotexistimsure")
      (load-foreign-library-error () 'error))
  error)

(define-foreign-library pseudo-library
  (t pseudo-library-spec))

;;; RT: T clause was being handled as :T by FEATUREP.
;;;
;;; We might want to export (and clean up) the API used in this test
;;; when the need arises.
(deftest library.t-clause
    (eq (cffi::foreign-library-spec
         (cffi::get-foreign-library 'pseudo-library))
        'pseudo-library-spec)
  t)

(define-foreign-library library-with-pathname
  (t #p"libdoesnotexistimsure"))

;;; RT: we were mishandling pathnames within libraries. (lp#1720626)
(deftest library.error.2
    (handler-case (load-foreign-library 'library-with-pathname)
      (load-foreign-library-error () 'error))
  error)

(deftest library.error.3
    (handler-case (load-foreign-library #p"libdoesnotexistimsure")
      (load-foreign-library-error () 'error))
  error)

;;;# Shareable Byte Vector Tests

#+ecl
(mapc (lambda (x) (pushnew x rtest::*expected-failures*))
      '(shareable-vector.1 shareable-vector.2))

(deftest shareable-vector.1
    (let ((vector (cffi-sys::make-shareable-byte-vector 5)))
      (cffi::with-pointer-to-vector-data (pointer vector)
        (strcpy pointer "xpto"))
      vector)
  #(120 112 116 111 0))

(deftest shareable-vector.2
    (block nil
      (let ((vector (cffi-sys::make-shareable-byte-vector 5)))
        (cffi::with-pointer-to-vector-data (pointer vector)
          (strcpy pointer "xpto")
          (return vector))))
  #(120 112 116 111 0))

;;; Inline foreign-type-size

(deftest foreign-type-size.inline.int
    (eql (foreign-type-size :int) (locally (declare (notinline foreign-type-size)) (foreign-type-size :int)))
  t)

(deftest foreign-type-size.inline.uint32
    (eql (foreign-type-size :uint32) (locally (declare (notinline foreign-type-size)) (foreign-type-size :uint32)))
  t)

(deftest foreign-type-size.inline.ptrdiff
    (eql (foreign-type-size :ptrdiff) (locally (declare (notinline foreign-type-size)) (foreign-type-size :ptrdiff)))
  t)

(deftest foreign-type-size.inline.size
    (eql (foreign-type-size :size) (locally (declare (notinline foreign-type-size)) (foreign-type-size :size)))
  t)

(deftest foreign-type-size.inline.offset
    (eql (foreign-type-size :offset) (locally (declare (notinline foreign-type-size)) (foreign-type-size :offset)))
  t)

(deftest foreign-type-size.inline.uintptr
    (eql (foreign-type-size :uintptr) (locally (declare (notinline foreign-type-size)) (foreign-type-size :uintptr)))
  t)

(deftest foreign-type-size.inline.intptr
    (eql (foreign-type-size :intptr) (locally (declare (notinline foreign-type-size)) (foreign-type-size :intptr)))
  t)
