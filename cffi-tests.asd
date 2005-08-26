;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-tests.asd --- ASDF system definition for CFFI unit tests.
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

(defpackage #:cffi-tests-system
  (:use #:cl #:asdf))
(in-package #:cffi-tests-system)

(defvar *tests-dir* (append (pathname-directory *load-truename*) '("tests")))

(defclass c-test-lib (c-source-file)
  ())

(defmethod perform ((o load-op) (c c-test-lib))
  nil)

(defmethod perform ((o compile-op) (c c-test-lib))
  #-(or win32 mswindows)
  (unless (zerop (run-shell-command
                   #-freebsd "cd ~A; make"
                   #+freebsd "cd ~A; gmake"
                   (namestring (make-pathname :name nil :type nil
                                              :directory *tests-dir*))))
    (error 'operation-error :component c :operation o)))

(defsystem cffi-tests
  :description "Unit tests for CFFI."
  :depends-on (cffi rt)
  :components
  ((:module "tests"
    :serial t
    :components
    ((:c-test-lib "libtest")
     (:file "package")
     (:file "bindings")
     (:file "funcall")
     (:file "defcfun")
     (:file "callbacks")
     (:file "foreign-globals")
     (:file "memory")
     (:file "struct")
     (:file "union")
     (:file "enum")
     (:file "misc-types")))))

(defmethod perform ((o test-op) (c (eql (find-system :cffi-tests))))
  (or (funcall (intern "DO-TESTS" '#:regression-test))
      (error "test-op failed.")))

;;; vim: ft=lisp et
