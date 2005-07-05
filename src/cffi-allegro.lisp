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
   #:foreign-var-ptr))

(in-package #:cffi-sys)

;;;# Basic Pointer Operations

;; TODO: figure out if there might be any issue with using
;; integers as pointers directly..

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (integerp ptr))
  ;(ff:foreign-pointer-p ptr))

(defun null-ptr ()
  "Return a null pointer."
  ;(ff:make-foreign-pointer :foreign-address 0))
  0)

(defun null-ptr-p (ptr)
  "Return true if PTR is a null pointer."
  ;(zerop (ff:foreign-pointer-address ptr)))
  (zerop ptr))

(defun inc-ptr (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  ;(ff:make-foreign-pointer
  ; :foreign-address (+ (ff:foreign-pointer-address ptr) offset)))
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
  ;; I *think* ff:allocate-fobject stack allocates
  `(let* ((,size-var ,size)
          (,var (foreign-alloc ,size-var)))
     (declare (ignorable ,size-var))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

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

;; Looking at UFFI, I see that this will probably
;; depend on the context --luis
(defun convert-foreign-type (type-keyword)
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
    (:pointer          :foreign-address)
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
  (ff:sizeof-fobject type-keyword))

;; FIXME: figure out which Allegro function has this info --luis
(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  ;; hmm, according to the docs this is always equal to the size
  ;; except for :double on Linux/x86 FreeBSD/x86 and RS/6000
  ;; nhe, I'm not sure about these features, couldn't find a list
  ;; --luis
  #+(or linux86 (and freebsd x86))
  (when (eq type-keyword :double)
    (return-from %foreign-type-alignment 4))
  (%foreign-type-size type-keyword))

(defun foreign-funcall-type-and-args (args)
  "Returns a list of types, list of args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
       if arg collect (convert-foreign-type type) into types
          and collect arg into fargs
       else do (setf return-type (convert-foreign-type type))
       finally (return (values types fargs return-type)))))

;(SYSTEM::FF-FUNCALL
; (LOAD-TIME-VALUE (EXCL::DETERMINE-FOREIGN-ADDRESS
;                   '("foo" :LANGUAGE :C)  2 NIL))
; '(:INT (INTEGER * *)) ARG1
; '(:DOUBLE (DOUBLE-FLOAT * *)) ARG2
; '(:INT (INTEGER * *)))

(defun convert-to-lisp-type (type)
  (ecase type
    ((:char :unsigned-char
      :short :unsigned-short
      :int :unsigned-int
      :long :unsigned-long) '(integer * *))
    (:float '(single-float * *))
    (:double '(double-float * *))
    (:foreign-address :foreign-address)
    (:void 'null)))

(defun foreign-allegro-type (type)
  (if (eq type :foreign-address)
      nil
      type))

(defmacro %foreign-funcall (name &rest args)
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(system::ff-funcall
      (load-time-value (excl::determine-foreign-address
                        '(,name :language :c) 2 nil))
      ,@(mapcan #'(lambda (type arg)
                    `('(,(foreign-allegro-type type)
                        ,(convert-to-lisp-type type))
                       ,arg))
                types fargs)
      '(,(foreign-allegro-type rettype)
        ,(convert-to-lisp-type rettype)))))

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  #+macosx
  ;; At least ACL 6.2 sets this to '("dylib") only.. not good.
  (let ((excl::*load-foreign-types* '("dylib" "so" "bundle")))
    (load name)) 
  #-macosx
  (load name))

;;;# Foreign Globals

;; FIXME: ff:get-entry-point will also grab functions
;; FIXME: doesn't seem to work on OSX?
(defun foreign-var-ptr (name)
  "Return a pointer pointing to the foreign variable NAME."
  (nth-value 0 (ff:get-entry-point name)))