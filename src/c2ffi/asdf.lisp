;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; asdf.lisp --- ASDF components for cffi/c2ffi.
;;;
;;; Copyright (C) 2015, Attila Lendvai <attila@lendvai.name>
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

(in-package #:cffi/c2ffi)

(defclass c2ffi-file (cl-source-file)
  ((package :initarg :package
            :initform nil
            :accessor c2ffi-file/package)
   (c2ffi-executable :initarg :c2ffi-executable
                     :accessor c2ffi-file/c2ffi-executable)
   (trace-c2ffi :initarg :trace-c2ffi
                :accessor c2ffi-file/trace-c2ffi)
   (prelude :initform nil
            :initarg :prelude
            :accessor c2ffi-file/prelude)
   (sys-include-paths :initarg :sys-include-paths
                      :initform nil
                      :accessor c2ffi-file/sys-include-paths)
   (exclude-archs :initarg :exclude-archs
                  :initform nil
                  :accessor c2ffi-file/exclude-archs)
   ;; The following slots correspond to an arg of the same name for
   ;; the generator function. No accessors are needed, they just hold
   ;; the data until it gets delegated to the generator function using
   ;; SLOT-VALUE and a LOOP.
   (ffi-name-transformer :initarg :ffi-name-transformer
                         :initform 'default-ffi-name-transformer)
   (ffi-type-transformer :initarg :ffi-type-transformer
                         :initform 'default-ffi-type-transformer)
   (foreign-library-name :initarg :foreign-library-name
                         :initform nil)
   (foreign-library-spec :initarg :foreign-library-spec
                         :initform nil)
   (emit-generated-name-mappings :initarg :emit-generated-name-mappings
                                 :initform :t)
   (rule-matcher :initform :cl-ppcre
                 :initarg :rule-matcher)
   (include-sources :initarg :include-sources
                    :initform :all)
   (exclude-sources :initarg :exclude-sources
                    :initform nil)
   (include-definitions :initarg :include-definitions
                        :initform :all)
   (exclude-definitions :initarg :exclude-definitions
                        :initform nil))
  (:default-initargs
   :type nil)
  (:documentation
   "The input of this ASDF component is a C header file, plus configuration for the
binding generation process. This header file will define the initial scope of
the generation process, which can further be filtered by other configuration
parameters.

An external program based on clang/llvm called 'c2ffi' is then invoked with
this header file to generate a json file for various platforms. The generation
of the underlying platform's json file must succeed to continue, but the
generation for the other platforms is allowed to fail
\(see ENSURE-SPEC-FILE-EXISTS).

It's advisable to deliver these json files with the project, so that users
don't need to have c2ffi installed.

Once/if the json file is available for the underlying platform, then the json
file is used to generate a lisp file with CFFI definitions (see
PROCESS-C2FFI-SPEC-FILE). This file will then be compiled as any other lisp
file, except that it's will be stored in the fasl cache."))

(defclass generate-op (downward-operation)
  ())

(defmethod input-files ((op generate-op) (c c2ffi-file))
  (list (component-pathname c)))

(defmethod input-files ((op compile-op) (c c2ffi-file))
  (list (first (output-files 'generate-op c))))

(defmethod component-depends-on ((op generate-op) (c c2ffi-file))
  `((prepare-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((op compile-op) (c c2ffi-file))
  `((generate-op ,c) ,@(call-next-method)))

(defmethod component-depends-on ((op load-source-op) (c c2ffi-file))
  `((generate-op ,c) ,@(call-next-method)))

(defmethod output-files ((op generate-op) (c c2ffi-file))
  (let* ((input-file (first (input-files op c)))
         (spec-file (spec-path input-file))
         (generated-lisp-file (make-pathname :type "lisp"
                                             :defaults spec-file)))
    (values
     (list*
      ;; NOTE: the order of the list elements influences various other
      ;; places in this file that call OUTPUT-FILE and take the NTH
      ;; member of the return value.
      ;; NOTE: applying ASDF:APPLY-OUTPUT-TRANSLATIONS here would
      ;; redirect the GENERATED-LISP-FILE into the fasl cache.
      generated-lisp-file
      ;; If we unconditionally returned the SPEC-FILE here as an output
      ;; file, that would make ASDF unconditionally redo the rest of the
      ;; generation and compilation.
      (unless (probe-file spec-file)
        (list spec-file)))
     ;; Tell ASDF not to apply output translation.
     t)))

(defmethod perform ((op generate-op) (c c2ffi-file))
  (let* ((output-file (first (output-files op c)))
         (input-file (first (input-files op c)))
         (spec-file (let ((*c2ffi-executable* (if (slot-boundp c 'c2ffi-executable)
                                                  (c2ffi-file/c2ffi-executable c)
                                                  *c2ffi-executable*))
                          (*trace-c2ffi* (if (slot-boundp c 'trace-c2ffi)
                                             (c2ffi-file/trace-c2ffi c)
                                             *trace-c2ffi*)))
                      (ensure-spec-file-exists
                       input-file
                       :exclude-archs (c2ffi-file/exclude-archs c)
                       :sys-include-paths (c2ffi-file/sys-include-paths c)))))
    (with-staging-pathname (tmp-output output-file)
      (format *debug-io* "~&; CFFI/C2FFI is generating the file ~S~%" output-file)
      (apply 'process-c2ffi-spec-file
             spec-file (c2ffi-file/package c)
             :output tmp-output
             :output-encoding (asdf:component-encoding c)
             :prelude (let ((prelude (c2ffi-file/prelude c)))
                        (if (and (pathnamep prelude)
                                 (not (absolute-pathname-p prelude)))
                            (merge-pathnames* prelude (component-pathname c))
                            prelude))
             ;; The following slots and keyword args have the same name in the ASDF
             ;; component and in PROCESS-C2FFI-SPEC-FILE, and this loop copies them.
             (loop
               :for arg :in '(ffi-name-transformer
                              ffi-type-transformer
                              foreign-library-name
                              foreign-library-spec
                              emit-generated-name-mappings
                              rule-matcher
                              include-sources
                              exclude-sources
                              include-definitions
                              exclude-definitions)
               :append (list (make-keyword arg)
                             (slot-value c arg)))))))

;; Allow for naked :cffi/c2ffi-file in asdf definitions.
(setf (find-class 'asdf::cffi/c2ffi-file) (find-class 'c2ffi-file))
