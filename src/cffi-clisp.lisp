;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-clisp.lisp --- CFFI-SYS implementation for CLISP.
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

;;;# A CLisp backend for CFFI.
;;;
;;; This is a preliminary port.  As of 7 June 2005 it looks like we'll
;;; need some things that are only present in the CVS version of CLISP
;;; (which doesn't compile for me at the moment).  Hopefully we will
;;; be able to fully support CLISP after the next release.

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
   #:foreign-symbol-pointer
   #:%defcallback))

(in-package #:cffi-sys)

;;; FIXME: long-long could be supported anyway on 64-bit machines. --luis

;;;# Features and Mis-*features*
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cffi/no-long-long *features*)
  #+macos (pushnew :darwin *features*)
  ;; XXX: this is probably also true for ppc64... --luis
  (when (equalp (machine-type) "POWER MACINTOSH")
    (pushnew :ppc32 *features*)))

;;;# Built-In Foreign Types

(defun convert-foreign-type (type)
  "Convert a CFFI built-in type keyword to a CLisp FFI type."
  (ecase type
    (:char 'ffi:char)
    (:unsigned-char 'ffi:uchar)
    (:short 'ffi:short)
    (:unsigned-short 'ffi:ushort)
    (:int 'ffi:int)
    (:unsigned-int 'ffi:uint)
    (:long 'ffi:long)
    (:unsigned-long 'ffi:ulong)
    (:int8 'ffi:sint8)
    (:uint8 'ffi:uint8)
    (:int16 'ffi:sint16)
    (:uint16 'ffi:uint16)
    (:int32 'ffi:sint32)
    (:uint32 'ffi:uint32)
    (:int64 'ffi:sint64)
    (:uint64 'ffi:uint64)
    (:float 'ffi:single-float)
    (:double 'ffi:double-float)
    ;; Clisp's FFI:C-POINTER converts NULL to NIL. For now
    ;; we have a workaround in the pointer operations...
    (:pointer 'ffi:c-pointer)
    (:void nil)))

(defun %foreign-type-size (type)
  "Return the size in bytes of objects having foreign type TYPE."
  (nth-value 0 (ffi:sizeof (convert-foreign-type type))))

;; Remind me to buy a beer for whoever made getting the alignment
;; of foreign types part of the public interface in CLisp. :-)
(defun %foreign-type-alignment (type)
  "Return the structure alignment in bytes of foreign TYPE."
  #+(and darwin ppc32)
  (when (eq type :double)
    (return-from %foreign-type-alignment 8))
  ;; Override not necessary for the remaining types...
  (nth-value 1 (ffi:sizeof (convert-foreign-type type))))

;;;# Basic Pointer Operations

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (or (null ptr) (typep ptr 'ffi:foreign-address)))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (eql (ffi:foreign-address-unsigned ptr1)
       (ffi:foreign-address-unsigned ptr2)))

(defun null-pointer ()
  "Return a null foreign pointer."
  (ffi:unsigned-foreign-address 0))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null foreign pointer."
  (or (null ptr) (zerop (ffi:foreign-address-unsigned ptr))))

(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (ffi:unsigned-foreign-address
   (+ offset (if (null ptr) 0 (ffi:foreign-address-unsigned ptr)))))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (ffi:unsigned-foreign-address address))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (ffi:foreign-address-unsigned ptr))

;;;# Foreign Memory Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of foreign-addressable memory and return a
pointer to the allocated block.  An implementation-specific error
is signalled if the memory cannot be allocated."
  (ffi:foreign-address (ffi:allocate-shallow 'ffi:uint8 :count size)))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC.  The results
are undefined if PTR is used after being freed."
  (ffi:foreign-free ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to a pointer to SIZE bytes of foreign-addressable
memory during BODY.  Both PTR and the memory block pointed to
have dynamic extent and may be stack allocated if supported by
the implementation.  If SIZE-VAR is supplied, it will be bound to
SIZE during BODY."
  (unless size-var
    (setf size-var (gensym)))
  (let ((obj-var (gensym)))
    `(let ((,size-var ,size))
       (ffi:with-foreign-object
           (,obj-var `(ffi:c-array ffi:uint8 ,,size-var))
         (let ((,var (ffi:foreign-address ,obj-var)))
           ,@body)))))

;;;# Memory Access

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference a pointer OFFSET bytes from PTR to an object of
built-in foreign TYPE.  Returns the object as a foreign pointer
or Lisp number."
  (let ((type (convert-foreign-type type)))
    (ffi:foreign-value
     (ffi::%offset
      (ffi:foreign-variable ptr type) offset type))))

(defun (setf %mem-ref) (value ptr type &optional (offset 0))
  "Set a pointer OFFSET bytes from PTR to an object of built-in
foreign TYPE to VALUE."
  (let ((type (convert-foreign-type type)))
    (setf (ffi:foreign-value
            (ffi::%offset (ffi:foreign-variable ptr type) offset type))
          value)))

;;;# Foreign Function Calling

(defun parse-foreign-funcall-args (args)
  "Return three values, a list of CLisp FFI types, a list of
values to pass to the function, and the CLisp FFI return type."
  (let ((return-type nil))
    (loop for (type arg) on args by #'cddr
          if arg collect (list (gensym) (convert-foreign-type type)) into types
             and collect arg into fargs
          else do (setf return-type (convert-foreign-type type))
          finally (return (values types fargs return-type)))))

(defmacro %foreign-funcall (name &rest args)
  "Invoke a foreign function called NAME, taking pairs of
foreign-type/value pairs from ARGS.  If a single element is left
over at the end of ARGS, it specifies the foreign return type of
the function call."
  (multiple-value-bind (types fargs rettype)
      (parse-foreign-funcall-args args)
    (let ((ctype `(ffi:c-function (:arguments ,@types)
                                   (:return-type ,rettype)
                                   (:language :stdc))))
      `(funcall
        (load-time-value
         (ffi::foreign-library-function
          ,name (ffi::foreign-library :default)
          nil (ffi:parse-c-type ',ctype)))
        ,@fargs))))

(defmacro %foreign-funcall-pointer (ptr &rest args)
  "Similar to %foreign-funcall but takes a pointer instead of a string."
  (multiple-value-bind (types fargs rettype)
      (parse-foreign-funcall-args args)
    `(funcall (ffi:foreign-function ,ptr (ffi:parse-c-type
                                          '(ffi:c-function
                                            (:arguments ,@types)
                                            (:return-type ,rettype)
                                            (:language :stdc))))
              ,@fargs)))

;;;# Callbacks

(defmacro %defcallback (name rettype arg-names arg-types &body body)
  (with-unique-names (cb-var)
    `(ffi:with-c-var
         (,cb-var '(ffi:c-ptr
                    (ffi:c-function
                     (:arguments
                      ,@(mapcar (lambda (sym type)
                                  (list sym (convert-foreign-type type)))
                                arg-names arg-types))
                     (:return-type ,(convert-foreign-type rettype))
                     (:language :stdc)))
                  (lambda ,arg-names ,@body))
       (setf (get ',name 'callback-ptr)
             (ffi:c-var-address (ffi:foreign-value ,cb-var))))))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name)
  "Load a foreign library from NAME."
  (ffi::foreign-library name))

(defun %close-foreign-library (name)
  "Close a foreign library NAME."
  (ffi:close-foreign-library name))

;;;# Foreign Globals

(defun foreign-symbol-pointer (name kind)
  "Returns a pointer to a foreign symbol NAME. KIND is one of
:CODE or :DATA, and is ignored on some platforms."
  (ecase kind
    (:data
     (prog1 (ignore-errors
              (ffi:c-var-address
               (ffi:foreign-value
                (ffi::foreign-library-variable
                 name (ffi::foreign-library :default) nil nil))))))
    (:code
     (prog1 (ignore-errors
              (ffi:c-var-address
               (ffi:foreign-value
                (ffi::foreign-library-function
                 name (ffi::foreign-library :default)
                 nil (ffi:parse-c-type '(ffi:c-function
                                         (:language :stdc)))))))))))