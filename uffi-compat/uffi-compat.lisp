;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; uffi-compat.lisp --- UFFI compatibility layer for CFFI.
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

(defpackage #:cffi-uffi-compat
  (:use #:cl)
  (:export
   #:def-type
   #:def-foreign-type
   #:def-constant
   #:null-char-p
   #:def-enum
   #:def-struct
   #:get-slot-value
   #:get-slot-pointer
   #:def-array-pointer
   #:deref-array
   #:def-union
   #:allocate-foreign-object
   #:free-foreign-object
   #:with-foreign-object
   #:size-of-foreign-type
   #:pointer-address
   #:deref-pointer
   #:ensure-char-character
   #:ensure-char-integer
   #:ensure-char-storage
   #:make-null-pointer
   #:null-pointer-p
   #:+null-cstring-pointer+
   #:with-cast-pointer
   #:convert-from-cstring
   #:convert-to-cstring
   #:free-cstring
   #:with-cstring
   #:convert-from-foreign-string
   #:def-foreign-var
   #:def-function))

(in-package #:cffi-uffi-compat)

(defun convert-uffi-type (uffi-type)
  "Convert a UFFI primitive type to a CFFI type."
  ;; Many CFFI types are the same as UFFI.  This list handles the
  ;; exceptions only.
  (case uffi-type
    (:cstring :pointer)
    (:pointer-void :pointer)
    (:pointer-self :pointer)
    (t
     (if (listp uffi-type)
         (case (car uffi-type)
           (* :pointer)
           ;; TODO: This function should accept an addition CONTEXT
           ;; parameter which handles :ARRAY types differently when
           ;; used as the argument to DEREF-ARRAY vs other macros.  It
           ;; also should probably return a second value indicating
           ;; the number of objects to allocate in certain situations.
           (:array (error "Array types are not yet supported.")))
         uffi-type))))

(defmacro def-type (name type)
  "Define a Common Lisp type NAME for UFFI type TYPE."
  (declare (ignore type))
  `(deftype ,name () t))

(defmacro def-foreign-type (name type)
  "Define a new foreign type."
  `(cffi:defctype ,name ,(convert-uffi-type type)))

(defmacro def-constant (name value &key export)
  "Define a constant and conditionally export it."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,@(when export `((export ',name)))
     ,'name))

(defmacro null-char-p (val)
  "Return true if character is null."
  `(zerop ,val))

(defmacro def-enum (enum-name args &key (separator-string "#"))
  "Creates a constants for a C type enum list, symbols are
created in the created in the current package. The symbol is the
concatenation of the enum-name name, separator-string, and
field-name"
  (let ((counter 0)
        (cmds nil)
        (constants nil))
    (declare (fixnum counter))
    (dolist (arg args)
      (let ((name (if (listp arg) (car arg) arg))
            (value (if (listp arg) 
                       (prog1
                           (setq counter (cadr arg))
                         (incf counter))
                       (prog1 
                           counter
                         (incf counter)))))
        (setq name (intern (concatenate 'string
                                        (symbol-name enum-name)
                                        separator-string
                                        (symbol-name name))))
        (push `(def-constant ,name ,value) constants)))
    (setf cmds (append '(progn) `((cffi:defctype ,enum-name :int))
                       (nreverse constants)))
    cmds))

(defmacro def-struct (name &body fields)
  "Define a C structure."
  `(cffi:defcstruct ,name
     ,@(loop for (name uffi-type) in fields
             for cffi-type = (convert-uffi-type uffi-type)
             collect (list name cffi-type))))

(defmacro get-slot-value (obj type field)
  "Access a slot value from a structure."
  `(cffi:foreign-slot-value ,obj ,type ,field))

(defmacro get-slot-pointer (obj type field)
  "Access a pointer slot value from a structure."
  (declare (ignore obj type field))
  (error "GET-SLOT-POINTER not implemented yet."))

(defmacro def-array-pointer (name type)
  "Define a foreign array type."
  (declare (ignore type))
  `(cffi:defctype ,name :pointer))

;;; This is very wrong.  TYPE actually contains the type of ARRAY, not
;;; the element type.  Unfortunately this means we have to crack the
;;; type open, possibly following typedefs to (:ARRAY ...)  types,
;;; which has no equivalent in CFFI.
;;;
;;; We will probably need to add a new subclass of FOREIGN-TYPE that
;;; stores array element-type information for UFFI array types.  I
;;; don't want to add such a type to CFFI proper.
(defmacro deref-array (array type position)
  "Dereference an array."
  `(cffi:foreign-aref ,array ,type ,position))

(defmacro def-union (name &body fields)
  "Define a foreign union type."
  (declare (ignore name fields))
  (error "DEF-UNION is not implemented yet."))

(defmacro allocate-foreign-object (type &optional (size 1))
  "Allocate one or more instance of a foreign type."
  `(cffi:foreign-object-alloc ,type ,size))

(defmacro free-foreign-object (ptr)
  "Free a foreign object allocated by ALLOCATE-FOREIGN-OBJECT."
  `(cffi:foreign-object-free ,ptr))

(defmacro with-foreign-object ((var type) &body body)
  "Wrap the allocation of a foreign object around BODY."
  `(cffi:with-foreign-object (,var ,(convert-uffi-type (eval type)))
     ,@body))

(defmacro size-of-foreign-type (type)
  "Return the size in bytes of a foreign type."
  `(cffi:foreign-type-size ,type))

(defmacro pointer-address (ptr)
  "Return the address of a pointer."
  (declare (ignore ptr))
  (error "POINTER-ADDRESS not implemented, who uses this?"))

(defmacro deref-pointer (ptr type)
  "Dereference a pointer."
  `(cffi:mem-ref ,ptr ,type))

(defmacro ensure-char-character (obj &environment env)
  "Convert OBJ to a character if it is an integer."
  (if (constantp obj env)
      (if (characterp obj) obj (code-char obj))
      (let ((obj-var (gensym)))
        `(let ((,obj-var ,obj))
           (if (characterp ,obj-var)
               ,obj-var
               (code-char ,obj-var))))))

(defmacro ensure-char-integer (obj &environment env)
  "Convert OBJ to an integer if it is a character."
  (if (constantp obj env)
      (if (characterp obj) (char-code obj) obj)
      (let ((obj-var (gensym)))
        `(let ((,obj-var ,obj))
           (if (characterp ,obj-var)
               (char-code ,obj-var)
               ,obj-var)))))

(defmacro ensure-char-storable (obj)
  "Ensure OBJ is storable as a character."
  `(ensure-char-integer ,obj))

(defmacro make-null-pointer (type)
  "Create a NULL pointer."
  (declare (ignore type))
  `(cffi:null-ptr))

(defmacro null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  `(cffi:null-ptr-p ,ptr))

(defparameter +null-cstring-pointer+ (cffi:null-ptr)
  "A constant NULL string pointer.")

(defmacro with-cast-pointer ((var ptr type) &body body)
  "Cast a pointer, does nothing in CFFI."
  (declare (ignore type))
  `(let ((,var ,ptr))
     ,@body))

(defmacro def-foreign-var (name type module)
  "Define a symbol macro to access a foreign variable."
  (declare (ignore name type module))
  (error "DEF-FOREIGN-VAR is not implemented yet."))

(defmacro convert-from-cstring (s)
  "Convert a cstring to a Lisp string."
  `(cffi:foreign-string-to-lisp ,s))

(defmacro convert-to-cstring (s)
  "Convert a Lisp string to a cstring."
  `(cffi:foreign-string-alloc ,s))

(defmacro free-cstring (ptr)
  "Free a cstring."
  `(cffi:foreign-string-free ,ptr))

(defmacro with-cstring ((var string) &body body)
  "Binds a newly creating string."
  `(cffi:with-foreign-string (,var ,string)
     ,@body))

(defmacro convert-from-foreign-string (s &key (length most-positive-fixnum)
                                       (null-terminated-p t))
  "Convert a foreign string to a Lisp string."
  `(cffi:foreign-string-to-lisp ,s ,length ,null-terminated-p))

(defmacro def-function (name args &key module (returning :void))
  "Define a foreign function."
  (declare (ignore module))
  `(cffi:defcfun ,name ,(convert-uffi-type returning)
     ,@(loop for (name type) in args
             collect `(,name ,(convert-uffi-type type)))))
