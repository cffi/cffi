;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; types.lisp --- Types of pointers in libffi
;;;
;;; Copyright (C) 2009, 2010, 2011 Liam Healy  <lhealy@common-lisp.net>
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

(in-package #:cffi-fsbv)

(cffi:defcvar ("ffi_type_double" +size-double+ :read-only t) :int)
(cffi:defcvar ("ffi_type_float" +size-float+ :read-only t) :int)
(cffi:defcvar ("ffi_type_longdouble" +size-longdouble+ :read-only t) :int)
(cffi:defcvar ("ffi_type_pointer" +size-pointer+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint16" +size-sint16+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint32" +size-sint32+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint64" +size-sint64+ :read-only t) :int)
(cffi:defcvar ("ffi_type_sint8" +size-sint8+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint16" +size-uint16+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint32" +size-uint32+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint64" +size-uint64+ :read-only t) :int)
(cffi:defcvar ("ffi_type_uint8" +size-uint8+ :read-only t) :int)
(cffi:defcvar ("ffi_type_void" +size-void+ :read-only t) :int)

(defmacro libffi-type-pointer (symbol)
  "Get the pointer into the libffi library that represents the type
   for the given symbol."
  `(get ',symbol 'type-pointer))

(defmacro defsynonym (name type)
  "Define a new name for an existing type."
  `(setf
    (libffi-type-pointer ,name)
    (libffi-type-pointer ,type)))

;;; Handle built-in types; see
;;; http://common-lisp.net/project/cffi/manual/html_node/Built_002dIn-Types.html#Built_002dIn-Types

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun ffi-builtin-name (type)
  "The libffi string from the built-in name of the CFFI type."
  (let ((str (string-downcase type)))
    (format nil "ffi_type_~a~a"
	    (if (string= str "int" :end1 3) "s" "")
	    str))))

(defmacro defcbuiltin (type)
  "Define the foreign object components reader and writer, assuming
   the cffi:mem-aref works on them."
  `(setf (libffi-type-pointer ,type)
	 (cffi:foreign-symbol-pointer ,(ffi-builtin-name type))))

(defcbuiltin :double)
(defcbuiltin :float)
(defcbuiltin :pointer)
(defcbuiltin :int8)
(defcbuiltin :int16)
(defcbuiltin :int32)
(defcbuiltin :int64)
(defcbuiltin :uint8)
(defcbuiltin :uint16)
(defcbuiltin :uint32)
(defcbuiltin :uint64)
(defcbuiltin :void)

;;; Assign these more accurately?
(defsynonym :char :int8)
(defsynonym :uchar :uint8)
(defsynonym :unsigned-char :uint8)
(defsynonym :short :int16)
(defsynonym :ushort :uint16)
(defsynonym :unsigned-short :uint16)
(defsynonym :int :int32)
(defsynonym :uint :uint32)
(defsynonym :long :int64)
(defsynonym :ulong :uint64)
(defsynonym :unsigned-long :uint64)
