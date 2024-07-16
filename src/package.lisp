;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for CFFI.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2009, Luis Oliveira  <loliveira@common-lisp.net>
;;; Copyright (C) 2012, Mark Evenson  <evenson.not.org@gmail.com>
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

#+abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :abcl-contrib)
  (require :jna)
  (require :jss))

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:common-lisp #:alexandria
        #+abcl #:java
        #+(or ccl mcl) #:ccl
        #+cmu #:alien #+cmu #:c-call
        #+corman #:c-types
        #+sbcl #:sb-alien)
  #+ecl
  (:import-from #:si #:null-pointer-p)
  (:shadow #:copy-file                  ; Conflicts with ccl:copy-file
           )
  (:export
   ;; Platform-specific functionality
   #+ecl #:*cffi-ecl-method*

   ;; C ABI utils
   #:canonicalize-symbol-name-case
   #:defcfun-helper-forms

   ;; Pointers
   #:foreign-pointer
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address

   ;; Memory operators
   #:%mem-ref
   #:%mem-set

   ;; Foreign symbols
   #:%foreign-symbol-pointer

   ;; Memory management
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer

   ;; Foreign functions
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-funcall-varargs
   #:%foreign-funcall-pointer-varargs
   #:%foreign-type-alignment

   ;; Foreign types
   #:%foreign-type-size

   ;; Foreign libraries
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring

   ;; Callbacks
   #:%defcallback
   #:%callback

   ;; Shareable vectors
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data

   ;; Compiler macro utils
   #:constant-form-p
   #:constant-form-value))

;;; Create a package to contain the symbols for callback functions.
;;; We want to redefine callbacks with the same symbol so the internal
;;; data structures are reused.
#+(or allegro ccl cmu ecl lispworks mkcl)
(defpackage #:cffi-callbacks
  (:use))

(defpackage #:cffi-features
  (:use #:cl)
  (:export
   #:cffi-feature-p

   ;; Features related to the CFFI-SYS backend.  Why no-*?  This
   ;; reflects the hope that these symbols will go away completely
   ;; meaning that at some point all lisps will support long-longs,
   ;; the foreign-funcall primitive, etc...
   #:no-long-long
   #:no-foreign-funcall
   #:no-stdcall
   #:flat-namespace

   ;; Only ECL and SCL support long-double...
   ;;#:no-long-double

   ;; Features related to the operating system.
   ;; More should be added.
   #:darwin
   #:unix
   #:windows

   ;; Features related to the processor.
   ;; More should be added.
   #:ppc32
   #:x86
   #:x86-64
   #:sparc
   #:sparc64
   #:hppa
   #:hppa64))

(defpackage #:cffi
  (:use #:common-lisp #:cffi-sys #:babel-encodings)
  (:import-from #:alexandria
                #:compose
                #:ensure-list
                #:featurep
                #:format-symbol
                #:hash-table-values
                #:if-let
                #:ignore-some-conditions
                #:lastcar
                #:make-gensym-list
                #:make-keyword
                #:mappend
                #:once-only
                #:parse-body
                #:simple-style-warning
                #:symbolicate
                #:unwind-protect-case
                #:when-let
                #:with-unique-names)
  (:export
   ;; Types.
   #:foreign-pointer

   ;; FIXME: the following types are undocumented. They should
   ;; probably be replaced with a proper type introspection API
   ;; though.
   #:*built-in-foreign-types*
   #:*other-builtin-types*
   #:*built-in-integer-types*
   #:*built-in-float-types*

   ;; Primitive pointer operations.
   #:foreign-free
   #:foreign-alloc
   #:mem-aptr
   #:mem-aref
   #:mem-ref
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:incf-pointer
   #:with-foreign-pointer
   #:make-pointer
   #:pointer-address

   ;; Shareable vectors.
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data

   ;; Foreign string operations.
   #:*default-foreign-encoding*
   #:foreign-string-alloc
   #:foreign-string-free
   #:foreign-string-to-lisp
   #:lisp-string-to-foreign
   #:with-foreign-string
   #:with-foreign-strings
   #:with-foreign-pointer-as-string

   ;; Foreign array operations.
   ;; TODO: document these
   #:foreign-array-alloc
   #:foreign-array-free
   #:foreign-array-to-lisp
   #:lisp-array-to-foreign
   #:with-foreign-array
   #:foreign-aref

   ;; Foreign function operations.
   #:defcfun
   #:foreign-funcall
   #:foreign-funcall-pointer
   #:foreign-funcall-varargs
   #:foreign-funcall-pointer-varargs
   #:translate-camelcase-name
   #:translate-name-from-foreign
   #:translate-name-to-foreign
   #:translate-underscore-separated-name

   ;; Foreign library operations.
   #:*foreign-library-directories*
   #:*darwin-framework-directories*
   #:foreign-library
   #:foreign-library-load-state
   #:foreign-library-name
   #:foreign-library-pathname
   #:foreign-library-type
   #:foreign-library-loaded-p
   #:list-foreign-libraries
   #:define-foreign-library
   #:load-foreign-library
   #:load-foreign-library-error
   #:use-foreign-library
   #:close-foreign-library
   #:reload-foreign-libraries

   ;; Callbacks.
   #:callback
   #:get-callback
   #:defcallback

   ;; Foreign type operations.
   #:defcstruct
   #:defcunion
   #:defctype
   #:defcenum
   #:defbitfield
   #:define-foreign-type
   #:define-parse-method
   #:define-c-struct-wrapper
   #:foreign-enum-keyword
   #:foreign-enum-keyword-list
   #:foreign-enum-value
   #:foreign-bitfield-symbol-list
   #:foreign-bitfield-symbols
   #:foreign-bitfield-value
   #:foreign-slot-pointer
   #:foreign-slot-value
   #:foreign-slot-type
   #:foreign-slot-offset
   #:foreign-slot-count
   #:foreign-slot-names
   #:foreign-type-alignment
   #:foreign-type-size
   #:with-foreign-object
   #:with-foreign-objects
   #:with-foreign-slots
   #:convert-to-foreign
   #:convert-from-foreign
   #:convert-into-foreign-memory
   #:free-converted-object
   #:translation-forms-for-class

   ;; Extensible foreign type operations.
   #:define-translation-method          ; FIXME: undocumented
   #:translate-to-foreign
   #:translate-from-foreign
   #:translate-into-foreign-memory
   #:free-translated-object
   #:expand-to-foreign-dyn
   #:expand-to-foreign
   #:expand-from-foreign
   #:expand-into-foreign-memory

   ;; Foreign globals.
   #:defcvar
   #:get-var-pointer
   #:foreign-symbol-pointer
   ))
