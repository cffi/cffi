;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enum.lisp --- Tests on C enums.
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

(defcenum numeros
  (:one 1)
  :two
  :three
  :four
  (:forty-one 41)
  :forty-two)

(defcfun "check_enums" :int
  (one numeros)
  (two numeros)
  (three numeros)
  (four numeros)
  (forty-one numeros)
  (forty-two numeros))

(deftest enum.1
    (check-enums :one :two :three 4 :forty-one :forty-two)
  1)

(defcenum another-boolean :false :true)
(defcfun "return_enum" another-boolean (x :int))

(deftest enum.2
    (and (eq :false (return-enum 0))
         (eq :true (return-enum 1)))
  t)

(defctype yet-another-boolean another-boolean)
(defcfun ("return_enum" return-enum2) yet-another-boolean
  (x yet-another-boolean))

(deftest enum.3
    (and (eq :false (return-enum2 :false))
         (eq :true (return-enum2 :true)))
  t)
