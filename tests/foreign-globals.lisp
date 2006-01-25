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

#-cffi-features:no-long-long
(progn
  (defcvar "var_long_long"          :long-long)
  (defcvar "var_unsigned_long_long" :unsigned-long-long))

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
    (null-pointer-p *var-pointer*)
  t)

(deftest foreign-globals.ref.string
    *var-string*
  "Hello, foreign world!")

#-cffi-features:no-long-long
(progn
  #+openmcl (push 'foreign-globals.set.long-long rt::*expected-failures*)

  (deftest foreign-globals.ref.long-long
      *var-long-long*
    -9223372036854775807)

  (deftest foreign-globals.ref.unsigned-long-long
      *var-unsigned-long-long*
    18446744073709551615))

;; The *.set.* tests restore the old values so that the *.ref.*
;; don't fail when re-run.
(defmacro with-old-value-restored ((place) &body body)
  (let ((old (gensym)))
    `(let ((,old ,place))
       (prog1
           (progn ,@body)
         (setq ,place ,old)))))

(deftest foreign-globals.set.int
    (with-old-value-restored (*var-int*)
      (setq *var-int* 42)
      *var-int*)
  42)

(deftest foreign-globals.set.string
    (with-old-value-restored (*var-string*)
      (setq *var-string* "Ehxosxangxo")
      (prog1
          *var-string*
        ;; free the string we just allocated
        (foreign-free (mem-ref (get-var-pointer '*var-string*) :pointer))))
  "Ehxosxangxo")

#-cffi-features:no-long-long
(deftest foreign-globals.set.long-long
    (with-old-value-restored (*var-long-long*)
      (setq *var-long-long* -9223000000000005808)
      *var-long-long*)
  -9223000000000005808)

(deftest foreign-globals.get-var-pointer.1
    (pointerp (get-var-pointer '*char-var*))
  t)

(deftest foreign-globals.get-var-pointer.2
    (mem-ref (get-var-pointer '*char-var*) :char)
  -127)
