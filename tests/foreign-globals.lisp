;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; foreign-globals.lisp --- Tests on foreign globals.
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

(in-package #:cffi-tests)

(defcvar ("var_char" *char-var*) :char)
(defcvar "var_unsigned_char"     :unsigned-char)
(defcvar "var_short"             :short)
(defcvar "var_unsigned_short"    :unsigned-short)
(defcvar "var_int"               :int)
(defcvar "var_unsigned_int"      :unsigned-int)
(defcvar "var_long"              :long)
(defcvar "var_unsigned_long"     :unsigned-long)
(defcvar "var_float"             :float)
(defcvar "var_double"            :double)
(defcvar "var_pointer"           :pointer)
(defcvar "var_string"            :string)

(deftest foreign-globals.ref.char
    *char-var*
  -127)

(deftest foreign-globals.ref.unsigned-char
    *var-unsigned-char*
  255)

(deftest foreign-globals.ref.short
    *var-short*
  -32767)

(deftest foreign-globals.ref.unsigned-short
    *var-unsigned-short*
  65535)

(deftest foreign-globals.ref.int
    *var-int*
  -32767)

(deftest foreign-globals.ref.unsigned-int
    *var-unsigned-int*
  65535)

(deftest foreign-globals.ref.long
    *var-long*
  -2147483647)

(deftest foreign-globals.ref.unsigned-long
    *var-unsigned-long*
  4294967295)

(deftest foreign-globals.ref.float
    *var-float*
  42.0)

(deftest foreign-globals.ref.double
    *var-double*
  42.0d0)

(deftest foreign-globals.ref.pointer
    (null-ptr-p *var-pointer*)
  t)

(deftest foreign-globals.ref.string
    *var-string*
  "Hello, foreign world!")

;; The *.set.* tests restore the old values so that the *.ref.*
;; don't fail when re-run.

(deftest foreign-globals.set.int
    (let ((old *var-int*))
      (setq *var-int* 42)
      (prog1
          *var-int*
        (setq *var-int* old)))
  42)

(deftest foreign-globals.set.string
    (let ((old *var-string*))
      (setq *var-string* "Ehxosxangxo") 
      (prog1
          *var-string*
        ;; free the old string
        (foreign-free (mem-ref (get-var-ptr '*var-string*) :pointer))
        (setq *var-string* old)))
  "Ehxosxangxo")

(deftest foreign-globals.get-var-ptr.1
    (pointerp (get-var-ptr '*char-var*))
  t)

(deftest foreign-globals.get-var-ptr.2
    (mem-ref (get-var-ptr '*char-var*) :char)
  -127)