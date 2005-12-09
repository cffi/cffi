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

(defun foreign-var-name (name)
  "Return the foreign var name of NAME."
  (etypecase name
    (list (first name))
    (string name)
    (symbol
     (let ((sn (substitute #\_ #\- (string-downcase (symbol-name name)))))
       (if (eql (char sn 0) #\*)
           ;; remove asterisks around the var name
           (subseq sn 1 (1- (length sn)))
           sn)))))

(defun get-var-pointer (symbol)
  "Return a pointer to the foreign global variable relative to SYMBOL."
  (foreign-symbol-pointer (get symbol 'foreign-var-name) :data))

;;; a) Should this be exported?
;;; b) should we define a more specific condition?
;;; --luis
(defun foreign-symbol-pointer-or-lose (foreign-name)
  "Like foreign-symbol-ptr but throws an error instead of
returning nil when foreign-name is not found."
  (or (foreign-symbol-pointer foreign-name :data)
      (error "Trying access undefined foreign variable ~S." foreign-name)))
  
(defmacro defcvar (name type &key read-only)
  "Define a foreign global variable."
  (let* ((lisp-name (lisp-var-name name))
         (foreign-name (foreign-var-name name))
         (fn (symbolicate "%VAR-ACCESSOR-" lisp-name))
         (ptype (parse-type type)))
    (when (aggregatep ptype) ; we can't really setf an aggregate type
      (setq read-only t))    ; at least not yet...
    `(progn
       ;; Save foreign-name for posterior access by get-var-ptr
       (setf (get ',lisp-name 'foreign-var-name) ,foreign-name)
       ;; Getter
       (defun ,fn ()
         ,(if (aggregatep ptype)
              ;; no dereference for aggregate types.
              `(foreign-symbol-pointer-or-lose ,foreign-name) 
              `(with-object-translated
                   (var (mem-ref (foreign-symbol-pointer-or-lose ,foreign-name)
                                 ',type)
                        ,type :from-c)
                 var)))
       ;; Setter
       (defun (setf ,fn) (value)
         ,(if read-only '(declare (ignore value)) (values))
         ,(if read-only
              `(error ,(format nil "Trying to modify read-only foreign var: ~A."
                               lisp-name))
              `(with-object-translated
                   (c-value value ,type :to-c)
                 (setf (mem-ref (foreign-symbol-pointer-or-lose ,foreign-name)
                                ',type)
                       c-value)
                 value)))
       ;; Symbol macro
       (define-symbol-macro ,lisp-name (,fn)))))