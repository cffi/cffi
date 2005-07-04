;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; foreign-vars.lisp --- High-level interface to foreign globals.
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

(in-package #:cffi)

;;;# Accessing Foreign Globals

(defun lisp-var-name (name)
  "Return the Lisp symbol for foreign var NAME."
  (etypecase name
    (list (second name))
    (string (intern
             (format nil "*~A*"
                     (string-upcase (substitute #\- #\_ name)))))))

;; TODO: also convert lisp-name -> foreign-name? --luis
(defun foreign-var-name (name)
  "Return the foreign var name of NAME."
  (etypecase name
    (list (first name))
    (string name)))

(defmacro defcvar (name &key type)
  "Define a foreign global variable."
  (let ((lisp-name (lisp-var-name name))
        (foreign-name (foreign-var-name name)))
    `(progn
       (setf (get ',lisp-name 'foreign-ptr-to-var)
             (foreign-var-ptr ,foreign-name))
       (define-symbol-macro ,lisp-name
         (mem-ref (get ',lisp-name 'foreign-ptr-to-var) ,type)))))

(defmacro get-var-ptr (var)
  "Return a pointer to the foreign global variable VAR."
  `(get ,var 'foreign-ptr-to-var))