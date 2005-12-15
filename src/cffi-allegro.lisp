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
  (:use #:common-lisp #:cffi-utils)
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
   ;#:make-shareable-byte-vector
   ;#:with-pointer-to-vector-data
   #:foreign-symbol-pointer
   #:defcfun-helper-forms
   #:%defcallback))

(in-package #:cffi-sys)

;;;# Features and Mis-*features*
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cffi/no-long-long *features*)
  #+macosx (pushnew :darwin *features*)
  ;; This is probably incorrect?
  #+powerpc (pushnew :ppc32 *features*))

;;;# Basic Pointer Operations

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (integerp ptr))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (eql ptr1 ptr2))

(defun null-pointer ()
  "Return a null pointer."
  0)

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (zerop ptr))

(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (+ ptr offset))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  address)

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  ptr)

;;;# Allocation
;;;
;;; Functions and macros for allocating foreign memory on the stack
;;; and on the heap.  The main CFFI package defines macros that wrap
;;; FOREIGN-ALLOC and FOREIGN-FREE in UNWIND-PROTECT for the common usage
;;; when the memory has dynamic extent.

(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  (ff:allocate-fobject :char :c size))

(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (ff:free-fobject ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let ((,size-var ,size))
     (declare (ignorable ,size-var))
     (ff:with-stack-fobject (,var :char :c ,size-var)
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
    (setf ptr (inc-pointer ptr offset)))
  (ff:fslot-value-typed (convert-foreign-type type) :c ptr))

(defun (setf %mem-ref) (value ptr type &optional (offset 0))
  "Set the object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-pointer ptr offset)))
  (setf (ff:fslot-value-typed (convert-foreign-type type) :c ptr) value))

;;;# Calling Foreign Functions

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (ff:sizeof-fobject (convert-foreign-type type-keyword)))

(defun %foreign-type-alignment (type-keyword)
  "Returns the alignment in bytes of a foreign type."
  #+(and powerpc macosx32)
  (when (eq type-keyword :double)
    (return-from %foreign-type-alignment 8))
  ;; No override necessary for the remaining types....
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
      'integer
      (ecase type
        (:char 'signed-byte)
        (:unsigned-char 'integer) ;'unsigned-byte)
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

(defun defcfun-helper-forms (name lisp-name rettype args types)
  "Return 2 values for DEFCFUN. A prelude form and a caller form."
  (let ((ff-name (intern (format nil "%cffi-foreign-function/~A" lisp-name))))
    (values
     `(ff:def-foreign-call (,ff-name ,name)
          ,(mapcar (lambda (ty) 
                     (let ((allegro-type (convert-foreign-type ty)))
                       (list (gensym) allegro-type
                             (convert-to-lisp-type allegro-type))))
                   types)
        :returning ,(allegro-type-pair
                     (convert-foreign-type rettype :funcall))
        ;; Don't use call-direct when there are no arguments.
        ,@(unless (null args) '(:call-direct t))
        :arg-checking nil
        :strings-convert nil)
     `(,ff-name ,@args))))

;;; See doc/allegro-internals.txt for a clue about entry-vec.
(defmacro %foreign-funcall-pointer (ptr &rest args)
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    (with-unique-names (entry-vec)
      `(let ((,entry-vec (excl::make-entry-vec-boa)))
         (setf (aref ,entry-vec 1) ,ptr) ; set jump address
         (system::ff-funcall
          ,entry-vec
          ;; arg types {'(:c-type lisp-type) argN}*
          ,@(mapcan (lambda (type arg)
                      `(',(allegro-type-pair type) ,arg))
                    types fargs)
          ;; return type '(:c-type lisp-type)
          ',(allegro-type-pair rettype)))))) 

;;;# Callbacks

;; TODO: figure out if we want a coerce/check-type here for the return
;; value since allegro's defun-foreign-callable doesn't specify a return
;; type. Also, need to understand the second arg to r-f-c. --luis
(defmacro %defcallback (name rettype arg-names arg-types &body body)
  (declare (ignore rettype))
  (with-unique-names (cb-sym)
    `(progn
       (ff:defun-foreign-callable ,cb-sym
           ,(mapcar (lambda (sym type) (list sym (convert-foreign-type type)))
                    arg-names arg-types)
         (declare (:convention :c))
         ,@body)
       (setf (get ',name 'callback-ptr)
             (ff:register-foreign-callable ',cb-sym :reuse t)))))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  #+macosx ; At least ACL 6.2 sets this to '("dylib") only.. not good.
  (let ((excl::*load-foreign-types* '("dylib" "so" "bundle")))
    (load name))
  #-macosx
  (load name))

(defun %close-foreign-library (name)
  "Close the foreign library NAME."
  (ff:unload-foreign-library name))

;;;# Foreign Globals

(defun convert-external-name (name)
  "Add an underscore to NAME if necessary for the ABI."
  #+macosx (concatenate 'string "_" name)
  #-macosx name)

(defun foreign-symbol-pointer (name kind)
  "Returns a pointer to a foreign symbol NAME. KIND is one of
:CODE or :DATA, and is ignored on some platforms."
  (declare (ignore kind))
  (prog1 (ff:get-entry-point (convert-external-name name))))