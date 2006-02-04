;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi.asd --- ASDF system definition for CFFI.
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

#-(or openmcl sbcl cmu clisp lispworks ecl allegro cormanlisp)
(error "Sorry, this Lisp is not yet supported.  Patches welcome!")

(defpackage #:cffi-system
  (:use #:cl #:asdf))
(in-package #:cffi-system)

(defsystem cffi
  :description "The Common Foreign Function Interface"
  :author "James Bielman  <jamesjb@jamesjb.com>"
  :version "0.9.0"
  :licence "MIT"
  :components 
  ((:module src
    :serial t
    :components
    ((:file "utils")
     (:file "features")
     #+openmcl    (:file "cffi-openmcl")
     #+sbcl       (:file "cffi-sbcl")
     #+cmu        (:file "cffi-cmucl")
     #+clisp      (:file "cffi-clisp")
     #+lispworks  (:file "cffi-lispworks")
     #+ecl        (:file "cffi-ecl")
     #+allegro    (:file "cffi-allegro")
     #+cormanlisp (:file "cffi-corman")
     (:file "package")
     (:file "libraries")
     (:file "early-types")
     (:file "types")
     (:file "enum")
     (:file "strings")
     (:file "functions")
     (:file "foreign-vars")))))

;; vim: ft=lisp et
