;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; functions.lisp --- High-level interface to foreign functions.
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

(in-package #:cffi)

;;;# Calling Foreign Functions
;;;
;;; FOREIGN-FUNCALL is the main primitive for calling foreign
;;; functions.  It converts each argument based on the installed
;;; translators for its type, then passes the resulting list to
;;; CFFI-SYS:%FOREIGN-FUNCALL.
;;;
;;; For implementation-specific reasons, DEFCFUN doesn't use
;;; FOREIGN-FUNCALL directly and might use something else
;;; (passed to TRANSLATE-OBJECTS as the CALL argument) instead
;;; of CFFI-SYS:%FOREIGN-FUNCALL to call the foreign-function.

(defmacro translate-objects (syms args types rettype call)
  "Helper macro for FOREIGN-FUNCALL and DEFCFUN."
  (cond
    ;; All arguments have been translated, translate
    ;; the return value and perform the call.
    ((null args)
     (let ((sym (gensym)))
       `(with-object-translated (,sym ,call ,rettype :result)
          ,sym)))
    ;; More than one argument is available---translate the first
    ;; argument/type pair and recurse.
    (t `(with-object-translated (,(car syms) ,(car args) ,(car types) :in)
          (translate-objects
           ,(rest syms) ,(rest args) ,(rest types) ,rettype ,call)))))

(defun parse-args-and-types (args)
  "Returns 4 values. Types, canonicalized types, args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect type into types
             and collect (canonicalize-foreign-type type) into ctypes
             and collect arg into fargs
          else do (setf return-type (canonicalize-foreign-type type))
          finally (return (values types ctypes fargs return-type)))))

(defmacro foreign-funcall (name &rest args)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments."
  (multiple-value-bind (types ctypes fargs rettype)
      (parse-args-and-types args)
    (let ((syms (loop repeat (length fargs) collect (gensym))))
      `(translate-objects ,syms ,fargs ,types ,rettype
                          (%foreign-funcall ,name
                                            ,@(mapcan #'list ctypes syms)
                                            ,rettype)))))

;;;# Defining Foreign Functions
;;;
;;; The DEFCFUN macro provides a declarative interface for defining
;;; Lisp functions that call foreign functions.

(defun lisp-function-name (name)
  "Return the Lisp function name for foreign function NAME."
  (etypecase name
    (list (second name))
    (string (intern (string-upcase (substitute #\- #\_ name))))))

(defun foreign-function-name (name)
  "Return the foreign function name of NAME."
  (etypecase name
    (list (first name))
    (string name)))

;; If cffi-sys doesn't provide a defcfun-helper-forms,
;; we define one that uses %foreign-funcall.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'defcfun-helper-forms)
    (defun defcfun-helper-forms (name rettype args types)
      (values
       '()
       `(%foreign-funcall ,name ,@(mapcan #'list types args) ,rettype)))))

(defmacro defcfun (name return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (let ((lisp-name (lisp-function-name name))
        (foreign-name (foreign-function-name name))
        (arg-names (mapcar #'car args))
        (arg-types (mapcar #'cadr args))
        (syms (loop repeat (length args) collect (gensym))))
    (multiple-value-bind (prelude caller)
        (defcfun-helper-forms
         foreign-name (canonicalize-foreign-type return-type)
         syms (mapcar #'canonicalize-foreign-type arg-types))
      `(progn
         ,prelude
         (defun ,lisp-name ,arg-names
           (translate-objects
            ,syms ,arg-names ,arg-types ,return-type ,caller))))))

;;;# Per-implementation DEFCFUN optimization

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (not (find-symbol "DEFCFUN-COMPILER-MACRO" '#:cffi-sys))
    (defun defcfun-compiler-macro (form &rest other-args)
      (declare (ignore other-args))
      (format nil "compiler macro returning: ~A" form) 
      form))))

(define-compiler-macro defcfun (&whole form name return-type &body args)
  (format nil "running compiler macro!")
  (defcfun-compiler-macro form name return-type args))
|#