;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; asdf.lisp --- ASDF components for cffi-grovel.
;;;
;;; Copyright (C) 2005-2006, Dan Knap <dankna@accela.net>
;;; Copyright (C) 2005-2006, Matthew Backes <lucca@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu <stelian.ionescu-zeus@poste.it>
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

(defclass grovel-file (asdf:cl-source-file cc-flags-mixin)
  ()
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-GROVEL-FILE in order
to generate a Lisp file that is subsequently compiled and/or
loaded."))

(defmethod asdf:perform ((op asdf:compile-op) (c grovel-file))
  (let ((output-file (ensure-pathname (car (asdf:output-files op c)))))
    (compile-file (process-grovel-file (asdf:component-pathname c) output-file)
                  :output-file output-file)))

(defmethod asdf:perform ((op asdf:load-source-op) (c grovel-file))
  (load (process-grovel-file
         (asdf:component-pathname c)
         (ensure-pathname (car (asdf:output-files op c))))))

;;;# ASDF component: WRAPPER-FILE

(defclass wrapper-file (asdf:cl-source-file cc-flags-mixin)
  ()
  (:documentation
   "This ASDF component defines COMPILE-OP and LOAD-SOURCE-OP
operations that take care of calling PROCESS-WRAPPER-FILE in
order to generate a foreign library and matching CFFI bindings
that are subsequently compiled and/or loaded."))

(defmethod asdf:perform ((op asdf:compile-op) (c wrapper-file))
  (let ((output-file (ensure-pathname (car (asdf:output-files op c)))))
    (compile-file (process-wrapper-file (asdf:component-pathname c) output-file)
                  :output-file output-file)))

(defmethod asdf:perform ((op asdf:load-source-op) (c wrapper-file))
  (load (process-wrapper-file
         (asdf:component-pathname c)
         (ensure-pathname (car (asdf:output-files op c))))))
