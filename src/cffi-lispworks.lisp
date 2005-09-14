;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-lispworks.lisp --- Lispworks CFFI-SYS implementation.
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
  (:use #:cl #:cffi-utils)
  (:export
   #:pointerp
   #:null-ptr
   #:null-ptr-p
   #:inc-ptr
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-ptr
   #:%foreign-funcall
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%mem-ref
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:foreign-var-ptr
   #:defcfun-helper-forms
   #:%defcallback))

(in-package #:cffi-sys)

;;;# Mis-*features*
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cffi/no-foreign-funcall *features*))

;;;# Basic Pointer Operations
(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (fli:pointerp ptr))

(defun null-ptr ()
  "Return a null foreign pointer."
  fli:*null-pointer*)

(defun null-ptr-p (ptr)
  "Return true if PTR is a null pointer."
  (fli:null-pointer-p ptr))

;; FLI:INCF-POINTER won't work on FLI pointers to :void so we
;; increment "manually."
(defun inc-ptr (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (fli:make-pointer :type :void :address (+ (fli:pointer-address ptr) offset)))

;;;# Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of memory and return a pointer."
  (fli:allocate-foreign-object :type :byte :nelems size))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC."
  (fli:free-foreign-object ptr))

(defmacro with-foreign-ptr ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  Both the
pointer in VAR and the memory it points to have dynamic extent and may
be stack allocated if supported by the implementation."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(fli:with-dynamic-foreign-objects ()
     (let* ((,size-var ,size)
            (,var (fli:alloca :type :byte :nelems ,size-var)))
       ,@body)))

;;;# Shareable Vectors

(defun make-shareable-byte-vector (size)
  "Create a shareable byte vector."
  (sys:in-static-area
    (make-array size :element-type '(unsigned-byte 8))))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a pointer at the data in VECTOR."
  `(fli:with-dynamic-lisp-array-pointer (,ptr-var ,vector)
     ,@body))

;;;# Dereferencing

(defun convert-foreign-type (cffi-type)
  "Convert a CFFI type keyword to an FLI type."
  (ecase cffi-type
    (:char            :byte)
    (:unsigned-char   '(:unsigned :byte))
    (:short           :short)
    (:unsigned-short  '(:unsigned :short))
    (:int             :int)
    (:unsigned-int    '(:unsigned :int))
    (:long            :long)
    (:unsigned-long   '(:unsigned :long))
    (:float           :float)
    (:double          :double)
    (:pointer         :pointer)
    (:void            :void)))

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of type TYPE OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-ptr ptr offset)))
  (fli:dereference ptr :type (convert-foreign-type type)))

(defun (setf %mem-ref) (value ptr type &optional (offset 0))
  "Set the object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-ptr ptr offset)))
  (setf (fli:dereference ptr :type (convert-foreign-type type)) value))

;;;# Foreign Type Operations

(defun %foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (fli:size-of (convert-foreign-type type)))

(defun %foreign-type-alignment (type)
  "Return the structure alignment in bytes of foreign type."
  (fli:align-of (convert-foreign-type type)))

;;;# Calling Foreign Functions

(defun foreign-funcall-type-and-args (args)
  "Returns a list of types, list of args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
       if arg collect (convert-foreign-type type) into types
          and collect arg into fargs
       else do (setf return-type (convert-foreign-type type))
       finally (return (values types fargs return-type)))))

(defmacro %foreign-funcall (name &rest args)
  "Call a foreign function NAME passing arguments ARGS."
  `(format t "~&;; Calling ~A with args ~S.~%" ,name ',args))

(defun defcfun-helper-forms (name lisp-name rettype args types)
  "Return 2 values for DEFCFUN. A prelude form and a caller form."
  (let ((ff-name (intern (format nil "%cffi-foreign-function/~A"  lisp-name))))
    (values
     `(fli:define-foreign-function (,ff-name ,name :source)
          ,(mapcar (lambda (ty) (list (gensym) (convert-foreign-type ty)))
                   types)
        :result-type ,(convert-foreign-type rettype)
        :language :ansi-c
        ;; avoid warning about cdecl not being supported on mac platforms
        #-mac ,@'(:calling-convention :cdecl))
     `(,ff-name ,@args))))

;;;# Callbacks

(defmacro %defcallback (name rettype arg-names arg-types &body body)
  (let ((cb-name (format nil "%callback/~A:~A"
                         (package-name (symbol-package name))
                         (symbol-name name))))
    `(progn
       (fli:define-foreign-callable
           (,cb-name :encode :source
                     :result-type ,(convert-foreign-type rettype)
                     :calling-convention :cdecl
                     :language :ansi-c
                     :no-check nil)
           ,(mapcar (lambda (sym type) (list sym (convert-foreign-type type)))
                    arg-names arg-types)
         ,@body)
       (setf (get ',name 'callback-ptr)
             (fli:make-pointer :symbol-name ,cb-name :module :callbacks)))))

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  (fli:register-module name))

;;;# Foreign Globals

(defmacro foreign-var-ptr (name)
  "Return a pointer pointing to the foreign variable NAME."
  `(load-time-value (fli:make-pointer :symbol-name ,name :type :void)))
