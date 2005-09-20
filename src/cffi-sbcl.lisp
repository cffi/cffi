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

;;; Notes:
;;;
;;; * requires SBCL >= 0.9.3 because SB-SYS:FOREIGN-SYMBOL-ADDRESS was
;;;   renamed to SB-SYS:FOREIGN-SYMBOL-SAP in SBCL 0.9.3 and prior versions
;;;   had a buggy SB-SYS:FOREIGN-SYMBOL-ADDRESS anyway. --luis

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:common-lisp #:sb-alien #:cffi-utils)
  (:export
   #:pointerp
   #:null-ptr
   #:null-ptr-p
   #:inc-ptr
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-ptr
   #:%foreign-funcall
   #:%foreign-funcall-ptr
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%mem-ref
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:foreign-symbol-ptr
   #:%defcallback))

(in-package #:cffi-sys)

;;;# Basic Pointer Operations

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (sb-sys:system-area-pointer-p ptr))

(defun null-ptr ()
  "Construct and return a null pointer."
  (sb-sys:int-sap 0))

(defun null-ptr-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop (sb-sys:sap-int ptr)))

(defun inc-ptr (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (sb-sys:sap+ ptr offset))

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

(defmacro with-foreign-ptr ((var size &optional size-var) &body body)
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
           (let ((,size-var ,size)
                 (,var (alien-sap ,alien-var)))
             (declare (ignorable ,size-var))
             ,@body)))
      `(let* ((,size-var ,size)
              (,var (%foreign-alloc ,size-var)))
         (declare (ignorable ,size-var))
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

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (ecase type
    (:char (sb-sys:signed-sap-ref-8 ptr offset))
    (:unsigned-char (sb-sys:sap-ref-8 ptr offset))
    (:short (sb-sys:signed-sap-ref-16 ptr offset))
    (:unsigned-short (sb-sys:sap-ref-16 ptr offset))
    (:int (sb-sys:signed-sap-ref-32 ptr offset))
    (:unsigned-int (sb-sys:sap-ref-32 ptr offset))
    (:long (sb-sys:signed-sap-ref-word ptr offset))
    (:unsigned-long (sb-sys:sap-ref-word ptr offset))
    (:float (sb-sys:sap-ref-single ptr offset))
    (:double (sb-sys:sap-ref-double ptr offset))
    (:pointer (sb-sys:sap-ref-sap ptr offset))))

(define-compiler-macro %mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code when TYPE is constant."
  (if (constantp type)
      (progn
        #-(and) (format t "~&;; Open-coding %MEM-REF form: ~S~%" form)
        (ecase (eval type)
          (:char `(sb-sys:signed-sap-ref-8 ,ptr ,offset))
          (:unsigned-char `(sb-sys:sap-ref-8 ,ptr ,offset))
          (:short `(sb-sys:signed-sap-ref-16 ,ptr ,offset))
          (:unsigned-short `(sb-sys:sap-ref-16 ,ptr ,offset))
          (:int `(sb-sys:signed-sap-ref-32 ,ptr ,offset))
          (:unsigned-int `(sb-sys:sap-ref-32 ,ptr ,offset))
          (:long `(sb-sys:signed-sap-ref-word ,ptr ,offset))
          (:unsigned-long `(sb-sys:sap-ref-word ,ptr ,offset))
          (:float `(sb-sys:sap-ref-single ,ptr ,offset))
          (:double `(sb-sys:sap-ref-double ,ptr ,offset))
          (:pointer `(sb-sys:sap-ref-sap ,ptr ,offset))))
      form))

(defun %mem-set (value ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (ecase type
    (:char (setf (sb-sys:signed-sap-ref-8 ptr offset) value))
    (:unsigned-char (setf (sb-sys:sap-ref-8 ptr offset) value))
    (:short (setf (sb-sys:signed-sap-ref-16 ptr offset) value))
    (:unsigned-short (setf (sb-sys:sap-ref-16 ptr offset) value))
    (:int (setf (sb-sys:signed-sap-ref-32 ptr offset) value))
    (:unsigned-int (setf (sb-sys:sap-ref-32 ptr offset) value))
    (:long (setf (sb-sys:signed-sap-ref-word ptr offset) value))
    (:unsigned-long (setf (sb-sys:sap-ref-word ptr offset) value))
    (:float (setf (sb-sys:sap-ref-single ptr offset) value))
    (:double (setf (sb-sys:sap-ref-double ptr offset) value))
    (:pointer (setf (sb-sys:sap-ref-sap ptr offset) value))))

(define-setf-expander %mem-ref (ptr type &optional (offset 0))
  "SETF expander for %MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on %MEM-SET to be able
to open-code (SETF %MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr)
    (declare (ignore setter newval))
    (let ((store (gensym)))
      (values
       dummies
       vals
       `(,store)
       `(progn
          (%mem-set ,store ,getter ,type ,offset)
          ,store)
       `(%mem-ref ,getter)))))

(define-compiler-macro %mem-set (&whole form value ptr type &optional (offset 0))
  "Compiler macro to open-code when TYPE is constant."
  (if (constantp type)
      (progn
        #-(and) (format t "~&;; Open-coding (SETF %MEM-REF) form: ~S~%" form)
        (ecase (eval type)
          (:char `(setf (sb-sys:signed-sap-ref-8 ,ptr ,offset) ,value))
          (:unsigned-char `(setf (sb-sys:sap-ref-8 ,ptr ,offset) ,value))
          (:short `(setf (sb-sys:signed-sap-ref-16 ,ptr ,offset) ,value))
          (:unsigned-short `(setf (sb-sys:sap-ref-16 ,ptr ,offset) ,value))
          (:int `(setf (sb-sys:signed-sap-ref-32 ,ptr ,offset) ,value))
          (:unsigned-int `(setf (sb-sys:sap-ref-32 ,ptr ,offset) ,value))
          (:long `(setf (sb-sys:signed-sap-ref-word ,ptr ,offset) ,value))
          (:unsigned-long `(setf (sb-sys:sap-ref-word ,ptr ,offset) ,value))
          (:float `(setf (sb-sys:sap-ref-single ,ptr ,offset) ,value))
          (:double `(setf (sb-sys:sap-ref-double ,ptr ,offset) ,value))
          (:pointer `(setf (sb-sys:sap-ref-sap ,ptr ,offset) ,value))))
      form))

;;;# Calling Foreign Functions

(defun convert-foreign-type (type-keyword)
  "Convert a CFFI type keyword to an SB-ALIEN type."
  (ecase type-keyword
    (:char             'char)
    (:unsigned-char    'unsigned-char)
    (:short            'short)
    (:unsigned-short   'unsigned-short)
    (:int              'int)
    (:unsigned-int     'unsigned-int)
    (:long             'long)
    (:unsigned-long    'unsigned-long)
    (:float            'single-float)
    (:double           'double-float)
    (:pointer          'system-area-pointer)
    (:void             'void)))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       (convert-foreign-type type-keyword) nil)) 8))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
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

(defmacro %foreign-funcall-ptr (ptr &rest args)
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

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  (load-shared-object name))

;;;# Foreign Globals

;; XXX still need to figure out eventual (non-)linkage-table issues?
(defun foreign-symbol-ptr (name kind)
  "Returns a pointer to a foreign symbol NAME. KIND is one of
:CODE or :DATA, and is ignored on some platforms."
  (sb-sys:foreign-symbol-sap name (ecase kind (:code nil) (:data t))))