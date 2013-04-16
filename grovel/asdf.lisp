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
  (declare (ignore op))
  (let ((*cc-flags* (append (ensure-list (cc-flags-of file))
                            *cc-flags*)))
    (call-next-method)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass process-op (asdf:operation)
    ()
    (:documentation "This ASDF operation performs the steps necessary
  to generate a compilable and loadable lisp file from a
  PROCESS-OP-INPUT component."))

  (defclass process-op-input (asdf:cl-source-file)
    ((generated-lisp-file-type
      :initarg :generated-lisp-file-type
      :accessor generated-lisp-file-type
      :documentation "The :TYPE argument to use for the generated lisp file."))
    (:default-initargs
     :generated-lisp-file-type "generated-lisp-file")
    (:documentation "This ASDF component represents a file that is
    used as input to a function that generates lisp source file. This
    component acts as if it is a CL-SOURCE-FILE by applying the
    COMPILE-OP and LOAD-SOURCE-OP operations to the file generated by
    PROCESS-OP.")))

(defmethod asdf:input-files ((op process-op) (c process-op-input))
  (list (asdf:component-pathname c)))

(defmethod asdf:component-depends-on ((op process-op) (c process-op-input))
  `(#-asdf3 (asdf:load-op ,@(asdf::component-load-dependencies c))
    #+asdf3 (asdf:prepare-op ,c)
    ,@(call-next-method)))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (c process-op-input))
  (declare (ignore op))
  `((process-op ,(asdf:component-name c))
    ,@(call-next-method)))

(defmethod asdf:component-depends-on ((op asdf:load-source-op) (c process-op-input))
  (declare (ignore op))
  `((process-op ,(asdf:component-name c))
    ,@(call-next-method)))

(defmethod asdf:perform ((op asdf:compile-op) (c process-op-input))
  (let ((generated-lisp-file (first (asdf:output-files (make-instance 'process-op) c))))
    (asdf:perform op (make-instance 'asdf:cl-source-file
                                    :name (asdf:component-name c)
                                    :parent (asdf:component-parent c)
                                    :pathname generated-lisp-file))))

(defmethod asdf:perform ((op asdf:load-source-op) (c process-op-input))
  (let ((generated-lisp-file (first (asdf:output-files (make-instance 'process-op) c))))
    (asdf:perform op (make-instance 'asdf:cl-source-file
                                    :name (asdf:component-name c)
                                    :parent (asdf:component-parent c)
                                    :pathname generated-lisp-file))))

;;;# ASDF component: GROVEL-FILE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass grovel-file (process-op-input cc-flags-mixin)
    ()
    (:default-initargs
     :generated-lisp-file-type "processed-grovel-file")
    (:documentation
     "This ASDF component represents an input file that is processed
     by PROCESS-GROVEL-FILE.")))

(defmethod asdf:output-files ((op process-op) (c grovel-file))
  (let* ((input-file (asdf:component-pathname c))
         (output-file (make-pathname :type (generated-lisp-file-type c)
                                     :defaults input-file))
         (c-file (make-c-file-name output-file)))
    (list output-file
          c-file
          (exe-filename c-file))))

(defmethod asdf:perform ((op process-op) (c grovel-file))
  (let ((output-file (first (asdf:output-files op c)))
        (input-file (asdf:component-pathname c)))
    (ensure-directories-exist (directory-namestring output-file))
    (let ((tmp-file (process-grovel-file input-file output-file)))
      (unwind-protect
           (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
        (delete-file tmp-file)))))

;;;# ASDF component: WRAPPER-FILE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass wrapper-file (process-op-input cc-flags-mixin)
    ((soname :initform nil :initarg :soname :accessor soname-of))
    (:default-initargs
     :generated-lisp-file-type "processed-wrapper-file")
    (:documentation
     "This ASDF component represents an input file that is processed
      by PROCESS-WRAPPER-FILE. This generates a foreign library and
      matching CFFI bindings that are subsequently compiled and
      loaded.")))

(defun wrapper-soname (c)
  (or (soname-of c)
      (asdf:component-name c)))

(defmethod asdf:output-files ((op process-op) (c wrapper-file))
  (let* ((input-file (asdf:component-pathname c))
         (output-file (make-pathname :type (generated-lisp-file-type c)
                                     :defaults input-file))
         (c-file (make-c-file-name output-file))
         (lib-soname (wrapper-soname c)))
    (list output-file
          c-file
          (lib-filename (make-soname lib-soname output-file)))))

(defmethod asdf:perform ((op process-op) (c wrapper-file))
  (let ((output-file (first (asdf:output-files op c)))
        (input-file  (asdf:component-pathname c)))
    (ensure-directories-exist (directory-namestring output-file))
    (let ((tmp-file (process-wrapper-file input-file output-file (wrapper-soname c))))
      (unwind-protect
           (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
        (delete-file tmp-file)))))

;; Allow for naked :grovel-file and :wrapper-file in asdf definitions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (find-class 'asdf::cffi-grovel-file) (find-class 'grovel-file))
  (setf (find-class 'asdf::cffi-wrapper-file) (find-class 'wrapper-file)))
