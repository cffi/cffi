;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-allegro.lisp --- CFFI-SYS implementation for Allegro CL.
;;;
;;; Copyright (C) 2005, Luis Oliveira  <loliveira(@)common-lisp.net>
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
  (:use #:common-lisp)
  (:export
   #:pointerp
   #:null-ptr
   #:null-ptr-p
   #:inc-ptr
   #:foreign-alloc
   #:foreign-free
   #:with-foreign-ptr
   #:%foreign-funcall
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%mem-ref
   ;#:make-shareable-byte-vector
   ;#:with-pointer-to-vector-data
   #:foreign-var-ptr
   #:defcfun-helper-forms))

(in-package #:cffi-sys)

;; Helper package where some foreign functions are kept.
(defpackage #:cffi-sys-ff)

;;;# Basic Pointer Operations

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (integerp ptr))

(defun null-ptr ()
  "Return a null pointer."
  0)

(defun null-ptr-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop ptr))

(defun inc-ptr (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (+ ptr offset))

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(defun foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (ff:allocate-fobject :char :c size))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (ff:free-fobject ptr))

(defmacro with-foreign-ptr ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let ((,size-var ,size))
     (declare (ignorable ,size-var))
     (ff:with-stack-fobject (,var :char :c ,size)
       ,@body)))
     
;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

;(defun make-shareable-byte-vector (size)
;  "Create a Lisp vector of SIZE bytes can passed to
;WITH-POINTER-TO-VECTOR-DATA."
;  (make-array size :element-type '(unsigned-byte 8)))
;
;(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
;  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
;  `(sb-sys:without-gcing
;     (let ((,ptr-var (sb-sys:vector-sap ,vector)))
;       ,@body)))

;;;# Dereferencing

(defun convert-foreign-type (type-keyword &optional (context :normal))
  "Convert a CFFI type keyword to an Allegro type."
  (ecase type-keyword
    (:char             :char)
    (:unsigned-char    :unsigned-char)
    (:short            :short)
    (:unsigned-short   :unsigned-short)
    (:int              :int)
    (:unsigned-int     :unsigned-int)
    (:long             :long)
    (:unsigned-long    :unsigned-long)
    (:float            :float)
    (:double           :double)
    (:pointer          (ecase context
                         (:normal '(* :void))
                         (:funcall :foreign-address)))
    (:void             :void)))

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-ptr ptr offset)))
  (ff:fslot-value-typed (convert-foreign-type type) :c ptr))

(defun (setf %mem-ref) (value ptr type &optional (offset 0))
  "Set the object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-ptr ptr offset)))
  (setf (ff:fslot-value-typed (convert-foreign-type type) :c ptr) value))

;;;# Calling Foreign Functions

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (ff:sizeof-fobject (convert-foreign-type type-keyword)))

(defun %foreign-type-alignment (type-keyword)
  (ff::sized-ftype-prim-align
   (ff::iforeign-type-sftype
    (ff:get-foreign-type
     (convert-foreign-type type-keyword)))))

(defun foreign-funcall-type-and-args (args)
  "Returns a list of types, list of args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
       if arg collect (convert-foreign-type type :funcall) into types
          and collect arg into fargs
       else do (setf return-type (convert-foreign-type type :funcall))
       finally (return (values types fargs return-type)))))

(defun convert-to-lisp-type (type)
  (if (equal '(* :void) type)
      '(* :void)
      (ecase type
        ((:char           
          :unsigned-char) 'unsigned-byte)
        ((:short
          :unsigned-short
          :int
          :unsigned-int
          :long
          :unsigned-long) 'integer)
        (:float 'single-float)
        (:double 'double-float)
        (:foreign-address :foreign-address)
        (:void 'null))))

(defun foreign-allegro-type (type)
  (if (eq type :foreign-address)
      nil
      type))

(defun allegro-type-pair (type)
  (list (foreign-allegro-type type)
        (convert-to-lisp-type type)))

#+ignore
(defun note-named-foreign-function (symbol name types rettype)
  "Give Allegro's compiler a hint to perform a direct call."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',symbol 'system::direct-ff-call)
           (list '(,name :language :c)
                 t  ; callback
                 :c ; convention
                 ;; return type '(:c-type lisp-type)
                 ',(allegro-type-pair (convert-foreign-type rettype :funcall))
                 ;; arg types '({(:c-type lisp-type)}*)
                 '(,@(loop for type in types
                           collect (allegro-type-pair
                                    (convert-foreign-type type :funcall))))
                 nil ; arg-checking
                 ff::ep-flag-never-release))))

(defmacro %foreign-funcall (name &rest args)
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(system::ff-funcall
      (load-time-value (excl::determine-foreign-address
                        '(,name :language :c)
                        ff::ep-flag-never-release
                        nil ; method-index
                        ))
      ;; arg types {'(:c-type lisp-type) argN}*
      ,@(mapcan (lambda (type arg)
                  `(',(allegro-type-pair type) ,arg))
                types fargs)
      ;; return type '(:c-type lisp-type)
      ',(allegro-type-pair rettype))))

(defun defcfun-helper-forms (name rettype args types)
  "Return 2 values for DEFCFUN. A prelude form and a caller form."
  (let ((ff-name (intern (format nil "~A%%~A" (length args) name)
                         '#:cffi-sys-ff)))
    (values
     `(ff:def-foreign-call (,ff-name ,name)
          ,(mapcar (lambda (ty) 
                     (let ((allegro-type (convert-foreign-type ty)))
                       (list (gensym) allegro-type
                             (convert-to-lisp-type allegro-type))))
                   types)
        :returning ,(allegro-type-pair
                     (convert-foreign-type rettype :funcall))
        :call-direct t
        :arg-checking nil
        :strings-convert nil)
     `(,ff-name ,@args))))

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  #+macosx ; At least ACL 6.2 sets this to '("dylib") only.. not good.
  (let ((excl::*load-foreign-types* '("dylib" "so" "bundle")))
    (load name))
  #-macosx
  (load name))

;;;# Foreign Globals

(defun convert-external-name (name)
  "Add an underscore to NAME if necessary for the ABI."
  #+macosx (concatenate 'string "_" name)
  #-macosx name)

(defmacro foreign-var-ptr (name)
  "Return a pointer pointing to the foreign variable NAME."
  `(load-time-value
    (nth-value 0 (ff:get-entry-point ,(convert-external-name name)))))

;  `(excl::foreign-global-address
;    (load-time-value
;     (excl::determine-foreign-address
;      '(,name :language :c)
;      ,(logior ff::ep-flag-never-release
;               ff::ep-flag-variable-address)))))

;(declare (ignore kind))
;(nth-value 0 (ff:get-entry-point (convert-external-name name))))