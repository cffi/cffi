;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-sbcl.lisp --- CFFI-SYS implementation for SBCL.
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

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:common-lisp #:sb-alien #:cffi-utils)
  (:export
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:%mem-ref
   #:%mem-set
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:foreign-symbol-pointer
   #:%defcallback))

(in-package #:cffi-sys)

;;;# Features

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc (lambda (feature) (pushnew feature *features*))
        '(;; Backend features.
          cffi-features:foreign-funcall
          cffi-features:long-long
          ;; OS/CPU features.
          #+darwin  cffi-features:darwin
          #+unix    cffi-features:unix
          #+win32   cffi-features:windows
          #+x86     cffi-features:x86
          #+(and ppc (not ppc64)) cffi-features:ppc32
          )))

;;;# Basic Pointer Operations

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (sb-sys:system-area-pointer-p ptr))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (sb-sys:sap= ptr1 ptr2))

(defun null-pointer ()
  "Construct and return a null pointer."
  (sb-sys:int-sap 0))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop (sb-sys:sap-int ptr)))

(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (sb-sys:sap+ ptr offset))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (sb-sys:int-sap address))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (sb-sys:sap-int ptr))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (alien-sap (make-alien (unsigned 8) size)))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (free-alien (sap-alien ptr (* (unsigned 8)))))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  ;; If the size is constant we can stack-allocate.
  (if (constantp size)
      (let ((alien-var (gensym "ALIEN")))
        `(with-alien ((,alien-var (array (unsigned 8) ,(eval size))))
           (let ((,size-var ,(eval size))
                 (,var (alien-sap ,alien-var)))
             (declare (ignorable ,size-var))
             ,@body)))
      `(let* ((,size-var ,size)
              (,var (%foreign-alloc ,size-var)))
         (unwind-protect
              (progn ,@body)
           (foreign-free ,var)))))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let ((vector-var (gensym "VECTOR")))
    `(let ((,vector-var ,vector))
       (sb-sys:with-pinned-objects (,vector-var)
         (let ((,ptr-var (sb-sys:vector-sap ,vector-var)))
           ,@body)))))

;;;# Dereferencing

;;; Define the %MEM-REF and %MEM-SET functions, as well as compiler
;;; macros that optimize the case where the type keyword is constant
;;; at compile-time.
(defmacro define-mem-accessors (&body pairs)
  `(progn
    (defun %mem-ref (ptr type &optional (offset 0))
      (ecase type
        ,@(loop for (keyword accessor) in pairs
                collect `(,keyword (,accessor ptr offset)))))
    (defun %mem-set (value ptr type &optional (offset 0))
      (ecase type
        ,@(loop for (keyword accessor) in pairs
                collect `(,keyword (setf (,accessor ptr offset) value)))))
    (define-compiler-macro %mem-ref
        (&whole form ptr type &optional (offset 0))
      (if (constantp type)
          (ecase (eval type)
            ,@(loop for (keyword accessor) in pairs
                    collect `(,keyword `(,',accessor ,ptr ,offset))))
          form))
    (define-compiler-macro %mem-set
        (&whole form value ptr type &optional (offset 0))
      (if (constantp type)
          (ecase (eval type)
            ,@(loop for (keyword accessor) in pairs
                    collect `(,keyword `(setf (,',accessor ,ptr ,offset)
                                         ,value))))
          form))))

(define-mem-accessors
  (:char sb-sys:signed-sap-ref-8)
  (:unsigned-char sb-sys:sap-ref-8)
  (:short sb-sys:signed-sap-ref-16)
  (:unsigned-short sb-sys:sap-ref-16)
  (:int sb-sys:signed-sap-ref-32)
  (:unsigned-int sb-sys:sap-ref-32)
  (:long sb-sys:signed-sap-ref-word)
  (:unsigned-long sb-sys:sap-ref-word)
  (:long-long sb-sys:signed-sap-ref-64)
  (:unsigned-long-long sb-sys:sap-ref-64)
  (:float sb-sys:sap-ref-single)
  (:double sb-sys:sap-ref-double)
  (:pointer sb-sys:sap-ref-sap))

;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an SB-ALIEN type."
  (ecase type-keyword
    (:char               'char)
    (:unsigned-char      'unsigned-char)
    (:short              'short)
    (:unsigned-short     'unsigned-short)
    (:int                'int)
    (:unsigned-int       'unsigned-int)
    (:long               'long)
    (:unsigned-long      'unsigned-long)
    (:long-long          'long-long)
    (:unsigned-long-long 'unsigned-long-long)
    (:float              'single-float)
    (:double             'double-float)
    (:pointer            'system-area-pointer)
    (:void               'void)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  #+(and darwin ppc (not ppc64))
  (when (member type-keyword '(:double :long-long))
    (return-from %foreign-type-alignment 8))
  ;; No override necessary for other types...
  (/ (sb-alien-internals:alien-type-alignment
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun foreign-funcall-type-and-args (args)
  "Return an SB-ALIEN function type for ARGS."
  (let ((return-type 'void))
    (loop for (type arg) on args by #'cddr
       if arg collect (convert-foreign-type type) into types
          and collect arg into fargs
       else do (setf return-type (convert-foreign-type type))
       finally (return (values types fargs return-type)))))

(defmacro %%foreign-funcall (name types fargs rettype)
  "Internal guts of %FOREIGN-FUNCALL."
  `(alien-funcall
    (extern-alien ,name (function ,rettype ,@types))
    ,@fargs))

(defmacro %foreign-funcall (name &rest args)
  "Perform a foreign function call, document it more later."
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(%%foreign-funcall ,name ,types ,fargs ,rettype)))

(defmacro %foreign-funcall-pointer (ptr &rest args)
  "Funcall a pointer to a foreign function."
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (function)
      `(with-alien ((,function (* (function ,rettype ,@types)) ,ptr))
         (alien-funcall ,function ,@fargs)))))

;;;# Callbacks

(defmacro %defcallback (name rettype arg-names arg-types &body body)
  `(setf (get ',name 'callback-ptr)
         (alien-sap
          (sb-alien::alien-lambda ,(convert-foreign-type rettype)
              ,(mapcar (lambda (sym type)
                         (list sym (convert-foreign-type type)))
                       arg-names arg-types)
            ,@body))))

#+nil
(defmacro make-callback (name rettype arg-names arg-types body-form)
  (declare (ignore name))
  `(alien-sap
    (sb-alien::alien-lambda ,(convert-foreign-type rettype)
        ,(mapcar (lambda (sym type) (list sym (convert-foreign-type type)))
                 arg-names arg-types)
      ,body-form)))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  (load-shared-object name))

(defun %close-foreign-library (name)
  "Closes the foreign library NAME."
  (sb-alien::dlclose-or-lose
   (find name sb-alien::*shared-objects*
         :key #'sb-alien::shared-object-file
         :test #'string=)))

;;;# Foreign Globals

;;; We return the address in the linkage-table (well, when there is one)
;;; in case someone uses this to save addresses somewhere and then dump
;;; an image. --luis
(defun foreign-symbol-pointer (name kind)
  "Returns a pointer to a foreign symbol NAME. KIND is one of
:CODE or :DATA, and is ignored on some platforms."
  (when (sb-sys:find-foreign-symbol-address name)
    (sb-sys:foreign-symbol-sap name (ecase kind (:code nil) (:data t)))))
