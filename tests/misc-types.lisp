;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; misc-types.lisp --- Various tests on the type system.
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

(defcfun ("my_strdup" strdup) :string+ptr (str :string))

(deftest misc-types.string+ptr
    (destructuring-bind (string pointer)
        (strdup "foo")
      (foreign-free pointer)
      string)
  "foo")

(defcfun "equalequal" :boolean
  (a (:boolean :int))
  (b (:boolean :unsigned-int)))

(defcfun "bool_and" (:boolean :char)
  (a (:boolean :unsigned-char))
  (b (:boolean :char)))

(defcfun "bool_xor" (:boolean :unsigned-long)
  (a (:boolean :long))
  (b (:boolean :unsigned-long)))

(deftest misc-types.boolean
    (list (equalequal nil nil)
          (equalequal t t)
          (equalequal t 23)
          (bool-and 'a 'b)
          (bool-and "foo" nil)
          (bool-xor t nil)
          (bool-xor nil nil))
  (t t t t nil t nil))

(defctype my-string :string+ptr)

(defun funkify (str)
  (concatenate 'string "MORE " (string-upcase str)))

(defun 3rd-person (value)
  (list (concatenate 'string "Strdup says: " (first value))
        (second value)))

;; (defctype funky-string
;;     (:wrapper my-string
;;               :to-c #'funkify
;;               :from-c (lambda (value)
;;                         (list
;;                          (concatenate 'string "Strdup says: "
;;                                       (first value))
;;                          (second value))))
;;   "A useful type.")

(defctype funky-string (:wrapper my-string :to-c funkify :from-c 3rd-person))

(defcfun ("my_strdup" funky-strdup) funky-string
  (str funky-string))

(deftest misc-types.wrapper
    (destructuring-bind (string ptr)
        (funky-strdup "code")
      (foreign-free ptr)
      string)
  "Strdup says: MORE CODE")

(deftest misc-types.sized-ints
    (mapcar #'foreign-type-size '(:int8 :uint8 :int16 :uint16 :int32 :uint32
                                  #-cffi-features:no-long-long :int64
                                  #-cffi-features:no-long-long :uint64))
  (1 1 2 2 4 4
   #-cffi-features:no-long-long 8
   #-cffi-features:no-long-long 8))

(defctype untranslated-int :int :translate-p nil)

(defmethod translate-to-foreign (value (type (eql 'untranslated-int)))
  (+ value 42))

(defmethod translate-from-foreign (value (type (eql 'untranslated-int)))
  (+ value 666))

(defcfun ("abs" untranslated-abs) untranslated-int
  (value untranslated-int))

;;; Ensure that type translators are not called on non-translatable
;;; typedefs when passing arguments or returning values to foreign
;;; functions.
(deftest misc-types.untranslated-typedef
    (untranslated-abs 1)
  1)

;;; Ensure that type translators are not called on non-translatable
;;; typedefs when passing values or returning from a callback.
#-cffi-features:no-foreign-funcall
(progn
  (defcallback untranslated-callback untranslated-int ((x untranslated-int))
    x)
  (deftest misc-types.untranslated-callback
      (foreign-funcall (callback untranslated-callback) :int 1 :int)
    1))
