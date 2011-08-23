;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; asdf.lisp --- ASDF components for cffi-grovel.
;;;
;;; Copyright (C) 2005-2006, Dan Knap <dankna@accela.net>
;;; Copyright (C) 2005-2006, Emily Backes <lucca@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu <sionescu@cddr.org>
;;; Copyright (C) 2007, Luis Oliveira <loliveira@common-lisp.net>
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

(in-package #:cffi-grovel)

(defun ensure-pathname (thing)
  (if (typep thing 'logical-pathname)
      (translate-logical-pathname thing)
      (pathname thing)))

(defclass cc-flags-mixin ()
  ((cc-flags :initform nil :accessor cc-flags-of :initarg :cc-flags)))

(defmethod asdf:perform :around ((op asdf:compile-op) (file cc-flags-mixin))
  (let ((*cc-flags* (append (ensure-list (cc-flags-of file))
                            *cc-flags*)))
    (call-next-method)))

;;;# ASDF component: GROVEL-FILE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass grovel-file (asdf:cl-source-file cc-flags-mixin)
    ()
    (:documentation
      "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-GROVEL-FILE in order
to generate a Lisp file that is subsequently compiled and/or
loaded.")))

(defmethod asdf:perform ((op asdf:compile-op) (c grovel-file))
  (let ((output-file (ensure-pathname (car (asdf:output-files op c)))))
    (compile-file (process-grovel-file (asdf:component-pathname c) output-file)
                  :output-file output-file
                  #+ecl :system-p #+ecl t)))

(defmethod asdf:perform ((op asdf:load-source-op) (c grovel-file))
  (load (process-grovel-file
         (asdf:component-pathname c)
         (ensure-pathname (car (asdf:output-files op c))))))

;;;# ASDF component: WRAPPER-FILE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass wrapper-file (asdf:cl-source-file cc-flags-mixin)
    ((soname :initform nil :initarg :soname :accessor soname-of))
    (:documentation
      "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-WRAPPER-FILE in
order to generate a foreign library and matching CFFI bindings
that are subsequently compiled and/or loaded.")))

(defun %perform-process-wrapper-file (op c)
  (let ((fasl-file (ensure-pathname (car (asdf:output-files op c)))))
    (values (process-wrapper-file (asdf:component-pathname c)
                                  fasl-file
                                  (or (soname-of c)
                                      (asdf:component-name c)))
            fasl-file)))

(defmethod asdf:perform ((op asdf:compile-op) (c wrapper-file))
  (multiple-value-bind (generated-source-file fasl-file)
      (%perform-process-wrapper-file op c)
    (compile-file generated-source-file
                  :output-file fasl-file
                  #+ecl :system-p #+ecl t)))

(defmethod asdf:perform ((op asdf:load-source-op) (c wrapper-file))
  (load (%perform-process-wrapper-file op c)))

;; Allow for naked :grovel-file and :wrapper-file in asdf definitions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'asdf::cffi-grovel-file) (find-class 'grovel-file))
  (setf (find-class 'asdf::cffi-wrapper-file) (find-class 'wrapper-file)))
