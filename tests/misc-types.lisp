;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; misc-types.lisp --- Various tests on the type system.
;;;
;;; Copyright (C) 2005-2006, Luis Oliveira  <loliveira(@)common-lisp.net>
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

(deftest misc-types.boolean.1
    (list (equalequal nil nil) 
          (equalequal t t)
          (equalequal t 23)
          (bool-and 'a 'b)
          (bool-and "foo" nil)
          (bool-xor t nil)
          (bool-xor nil nil))
  (t t t t nil t nil))


;;; Regression test: boolean type only worked with canonicalized
;;; built-in integer types. Should work for any type that canonicalizes
;;; to a built-in integer type.
(defctype int-for-bool :int)
(defcfun ("equalequal" equalequal2) :boolean
  (a (:boolean int-for-bool))
  (b (:boolean :uint)))

(deftest misc-types.boolean.2
    (equalequal2 nil t)
  nil)

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

(defctype error-error :int)

(defmethod translate-to-foreign (value (name (eql 'error-error)))
  (declare (ignore value))
  (error "translate-to-foreign invoked."))

(defmethod translate-from-foreign (value (name (eql 'error-error)))
  (declare (ignore value))
  (error "translate-from-foreign invoked."))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmethod expand-to-foreign (value (name (eql 'error-error)))
    value)

  (defmethod expand-from-foreign (value (name (eql 'error-error)))
    value))

(defcfun ("abs" expand-abs) error-error
  (n error-error))

(defcvar ("var_int" *expand-var-int*) error-error)

(defcfun ("expect_int_sum" expand-expect-int-sum) :boolean
  (cb :pointer))

(defcallback expand-int-sum error-error ((x error-error) (y error-error))
  (+ x y))

;;; Ensure that macroexpansion-time translators are called where this
;;; is guaranteed (defcfun, defcvar, foreign-funcall and defcallback)
(deftest misc-types.expand.1
    (expand-abs -1)
  1)

#-cffi-features:no-foreign-funcall
(deftest misc-types.expand.2
    (foreign-funcall "abs" error-error -1 error-error)
  1)

(deftest misc-types.expand.3
    (let ((old (mem-ref (get-var-pointer '*expand-var-int*) :int)))
      (unwind-protect
           (progn
             (setf *expand-var-int* 42)
             *expand-var-int*)
        (setf (mem-ref (get-var-pointer '*expand-var-int*) :int) old)))
  42)

(deftest misc-types.expand.4
    (expand-expect-int-sum (callback expand-int-sum))
  t)

(defctype translate-tracker :int)

(declaim (special .fto-called.))

(defmethod free-translated-object (value (type-name (eql 'translate-tracker))
                                   param)
  (declare (ignore value param))
  (setf .fto-called. t))

(defctype expand-tracker :int)

(defmethod free-translated-object (value (type-name (eql 'expand-tracker))
                                   param)
  (declare (ignore value param))
  (setf .fto-called. t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-to-foreign (value (type-name (eql 'expand-tracker)))
    (declare (ignore value))
    (call-next-method)))

(defcfun ("abs" ttracker-abs) :int
  (n translate-tracker))

(defcfun ("abs" etracker-abs) :int
  (n expand-tracker))

;; free-translated-object must be called when there is no etf
(deftest misc-types.expand.5
    (let ((.fto-called. nil))
      (ttracker-abs -1)
      .fto-called.)
  t)

;; free-translated-object must not be called when there is an etf, but
;; they answer *runtime-translator-form*
(deftest misc-types.expand.6
    (let ((.fto-called. nil))
      (etracker-abs -1)
      .fto-called.)
  nil)
