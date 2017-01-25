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

(defclass cc-flags-mixin ()
  ((cc-flags :initform nil :accessor cc-flags-of :initarg :cc-flags)))

(defmethod perform :around ((op compile-op) (file cc-flags-mixin))
  (let ((*cc-flags* (append (ensure-list (cc-flags-of file))
                            *cc-flags*)))
    (call-next-method)))

(defclass process-op (downward-operation)
  ()
  (:documentation "This ASDF operation performs the steps necessary
  to generate a compilable and loadable lisp file from a
  PROCESS-OP-INPUT component."))

(defclass process-op-input (cl-source-file)
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
    PROCESS-OP."))

(defmethod input-files ((op process-op) (c process-op-input))
  (list (component-pathname c)))

(defmethod input-files ((op compile-op) (c process-op-input))
  (list (first (output-files 'process-op c))))

(defmethod component-depends-on ((op process-op) (c process-op-input))
  `((prepare-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((op compile-op) (c process-op-input))
  `((process-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((op load-source-op) (c process-op-input))
  `((process-op ,c) ,@(call-next-method)))

;;;# ASDF component: GROVEL-FILE

(defclass grovel-file (process-op-input cc-flags-mixin)
  ((cache-dir :initform nil :initarg :cache-dir :accessor cache-dir-of))
  (:default-initargs
   :generated-lisp-file-type "processed-grovel-file")
  (:documentation
   "This ASDF component represents an input file that is processed
    by PROCESS-GROVEL-FILE."))

(defmethod output-files ((op process-op) (c grovel-file))
  (let* ((input-file (first (input-files op c)))
         (output-file (make-pathname :type (generated-lisp-file-type c)
                                     :defaults input-file))
         (c-file (make-c-file-name output-file "__grovel"))
         (exe-file (make-exe-file-name c-file)))
    (list output-file
          c-file
          exe-file)))


(defmethod perform ((op process-op) (c grovel-file))
  (destructuring-bind (output-file c-file exe-file) (output-files op c)
    (let* ((input-file (first (input-files op c)))
           (absolute-cache-dir (absolute-cache-dir input-file (cache-dir-of c))))
      (process-grovel-file* input-file output-file c-file exe-file absolute-cache-dir))))


;;;# ASDF component: WRAPPER-FILE

(defclass wrapper-file (process-op-input cc-flags-mixin)
  ((soname :initform nil :initarg :soname :accessor soname-of)
   (cache-dir :initform nil :initarg :cache-dir :accessor cache-dir-of))
  (:default-initargs
   :generated-lisp-file-type "processed-wrapper-file")
  (:documentation
   "This ASDF component represents an input file that is processed
    by PROCESS-WRAPPER-FILE. This generates a foreign library and
    matching CFFI bindings that are subsequently compiled and
    loaded."))

(defun wrapper-soname (c)
  (or (soname-of c)
      (component-name c)))

(defmethod output-files ((op process-op) (c wrapper-file))
  (let* ((input-file (first (input-files op c)))
         (output-file (make-pathname :type (generated-lisp-file-type c)
                                     :defaults input-file))
         (c-file (make-c-file-name output-file "__wrapper"))
         (o-file (make-o-file-name output-file "__wrapper"))
         (lib-soname (wrapper-soname c))
         (lib-file (make-lib-file-name (make-soname lib-soname output-file))))
    (list output-file
          lib-file
          c-file
          o-file)))

;;; Declare the .o and .so files as compilation outputs,
;;; so they get picked up by bundle operations.
#.(when (version<= "3.1.6" (asdf-version))
    '(defmethod output-files ((op compile-op) (c wrapper-file))
      (destructuring-bind (generated-lisp lib-file c-file o-file) (output-files 'process-op c)
        (declare (ignore generated-lisp c-file))
        (multiple-value-bind (files translatedp) (call-next-method)
          (values (append files (list lib-file o-file)) translatedp)))))

(defun absolute-cache-dir (input-file cache-dir)
  (when cache-dir
    (let ((input-dir (uiop:pathname-directory-pathname input-file)))
      (uiop:ensure-directory-pathname
       (uiop:subpathname input-dir cache-dir)))))

(defmethod perform ((op process-op) (c wrapper-file))
  (destructuring-bind (output-file lib-name c-file o-file) (output-files op c)
    (let* ((input-file (first (input-files op c)))
           (absolute-cache-dir (absolute-cache-dir input-file (cache-dir-of c))))
      (process-wrapper-file
       input-file
       output-file lib-name c-file o-file
       :lib-soname (wrapper-soname c)
       :absolute-cache-dir absolute-cache-dir))))


;; Allow for naked :grovel-file and :wrapper-file in asdf definitions.
(setf (find-class 'asdf::cffi-grovel-file) (find-class 'grovel-file))
(setf (find-class 'asdf::cffi-wrapper-file) (find-class 'wrapper-file))
