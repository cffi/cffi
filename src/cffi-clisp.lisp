;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-clisp.lisp --- CFFI-SYS implementation for CLISP.
;;;
;;; Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
;;;           (C) 2005, Joerg Hoehle  <hoehle@users.sourceforge.net>
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
   #:canonicalize-symbol-name-case
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
   #:foreign-symbol-pointer
   #:%defcallback
   #:%callback
   #:finalize
   #:cancel-finalization))

(in-package #:cffi-sys)

;;; FIXME: long-long could be supported anyway on 64-bit machines. --luis

;;;# Features

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc (lambda (feature) (pushnew feature *features*))
        '(;; Backend mis-features.
          cffi-features:no-long-long
          ;; OS/CPU features.
          #+macos  cffi-features:darwin
          #+unix   cffi-features:unix
          #+win32  cffi-features:windows
          ))
  (cond ((string-equal (machine-type) "X86_64")
         (pushnew 'cffi-features:x86-64 *features*))
        ((member :pc386 *features*)
         (pushnew 'cffi-features:x86 *features*))
        ;; FIXME: probably catches PPC64 as well
        ((string-equal (machine-type) "POWER MACINTOSH")
         (pushnew 'cffi-features:ppc32 *features*))))

;;; Symbol case.

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

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
  #+(and cffi-features:darwin cffi-features:ppc32)
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
    (setf size-var (gensym "SIZE")))
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
  (ffi:memory-as ptr (convert-foreign-type type) offset))

(define-compiler-macro %mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code when TYPE is constant."
  (if (constantp type)
      `(ffi:memory-as ,ptr ',(convert-foreign-type (eval type)) ,offset)
      form))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set a pointer OFFSET bytes from PTR to an object of built-in
foreign TYPE to VALUE."
  (setf (ffi:memory-as ptr (convert-foreign-type type) offset) value))

(define-compiler-macro %mem-set
    (&whole form value ptr type &optional (offset 0))
  (if (constantp type)
      ;; (setf (ffi:memory-as) value) is exported, but not so nice
      ;; w.r.t. the left to right evaluation rule
      `(ffi::write-memory-as ,value ,ptr ',(convert-foreign-type (eval type)) ,offset)
      form))

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
         (multiple-value-bind (ff error)
             (ignore-errors
               (ffi::foreign-library-function
                ,name (ffi::foreign-library :default)
                nil (ffi:parse-c-type ',ctype)))
           (or ff
               (warn (format nil "~?"
                             (simple-condition-format-control error)
                             (simple-condition-format-arguments error)))))) 
        ,@fargs))))

(defmacro %foreign-funcall-pointer (ptr &rest args)
  "Similar to %foreign-funcall but takes a pointer instead of a string."
  (multiple-value-bind (types fargs rettype)
      (parse-foreign-funcall-args args)
    `(funcall (ffi:foreign-function ,ptr
                                    (load-time-value
                                     (ffi:parse-c-type
                                      '(ffi:c-function
                                        (:arguments ,@types)
                                        (:return-type ,rettype)
                                        (:language :stdc)))))
              ,@fargs)))

;;;# Callbacks

;;; *CALLBACKS* contains the callbacks defined by the CFFI DEFCALLBACK
;;; macro.  The symbol naming the callback is the key, and the value
;;; is a list containing a Lisp function, the parsed CLISP FFI type of
;;; the callback, and a saved pointer that should not persist across
;;; saved images.
(defvar *callbacks* (make-hash-table))

;;; Return a CLISP FFI function type for a CFFI callback function
;;; given a return type and list of argument names and types.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun callback-type (rettype arg-names arg-types)
    (ffi:parse-c-type
     `(ffi:c-function
       (:arguments ,@(mapcar (lambda (sym type)
                               (list sym (convert-foreign-type type)))
                             arg-names arg-types))
       (:return-type ,(convert-foreign-type rettype))
       (:language :stdc)))))

;;; Register and create a callback function.
(defun register-callback (name function parsed-type)
  (setf (gethash name *callbacks*)
        (list function parsed-type
              (ffi:with-foreign-object (ptr 'ffi:c-pointer)
                ;; Create callback by converting Lisp function to foreign
                (setf (ffi:memory-as ptr parsed-type) function)
                (ffi:foreign-value ptr)))))

;;; Restore all saved callback pointers when restarting the Lisp
;;; image.  This is pushed onto CUSTOM:*INIT-HOOKS*.
;;; Needs clisp > 2.35, bugfix 2005-09-29
(defun restore-callback-pointers ()
  (maphash
   (lambda (name list)
     (register-callback name (first list) (second list)))
   *callbacks*))

;;; Add RESTORE-CALLBACK-POINTERS to the lists of functions to run
;;; when an image is restarted.
(eval-when (:load-toplevel :execute)
  (pushnew 'restore-callback-pointers custom:*init-hooks*))

;;; Define a callback function NAME to run BODY with arguments
;;; ARG-NAMES translated according to ARG-TYPES and the return type
;;; translated according to RETTYPE.  Obtain a pointer that can be
;;; passed to C code for this callback by calling %CALLBACK.
(defmacro %defcallback (name rettype arg-names arg-types &body body)
  `(register-callback ',name (lambda ,arg-names ,@body)
                      ,(callback-type rettype arg-names arg-types)))

;;; Look up the name of a callback and return a pointer that can be
;;; passed to a C function.  Signals an error if no callback is
;;; defined called NAME.
(defun %callback (name)
  (multiple-value-bind (list winp) (gethash name *callbacks*)
    (unless winp
      (error "Undefined callback: ~S" name))
    (third list)))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name)
  "Load a foreign library from NAME."
  (ffi::foreign-library name))

(defun %close-foreign-library (name)
  "Close a foreign library NAME."
  (ffi:close-foreign-library name))

;;;# Foreign Globals

(defun foreign-symbol-pointer (name)
  "Returns a pointer to a foreign symbol NAME."
  (prog1 (ignore-errors
           (ffi:foreign-address
            (ffi::foreign-library-variable
             name (ffi::foreign-library :default) nil nil)))))

;;;# Finalizers

(defvar *finalizers* (make-hash-table :test 'eq :weak :key)
  "Weak hashtable that holds registered finalizers.")

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
finalizers. FUNCTION should take no arguments. Returns OBJECT.

For portability reasons, FUNCTION should not attempt to look at
OBJECT by closing over it because, in some lisps, OBJECT will
already have been garbage collected and is therefore not
accessible when FUNCTION is invoked."
  (push function (gethash object *finalizers*))
  (ext:finalize object
                (lambda (obj)
                  (mapc #'funcall (gethash obj *finalizers*))))
  object)

(defun cancel-finalization (object)
  "Cancels all of OBJECT's finalizers, if any."
  (remhash object *finalizers*))
