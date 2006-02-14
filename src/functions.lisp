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

(defun translate-objects (syms args types rettype call-form)
  "Helper function for FOREIGN-FUNCALL and DEFCFUN."
  (if (null args)
      (expand-type-from-foreign call-form (parse-type rettype))
      (expand-type-to-foreign-dyn
       (car args) (car syms)
       (list (translate-objects (cdr syms) (cdr args)
                                (cdr types) rettype call-form))
       (parse-type (car types)))))

(defun parse-args-and-types (args)
  "Returns 4 values. Types, canonicalized types, args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect type into types
             and collect (canonicalize-foreign-type type) into ctypes
             and collect arg into fargs
          else do (setf return-type type)
          finally (return (values types ctypes fargs return-type)))))

(defmacro foreign-funcall (name-or-pointer &rest args)
  "Wrapper around %FOREIGN-FUNCALL(-POINTER) that translates its arguments."
  (multiple-value-bind (types ctypes fargs rettype)
      (parse-args-and-types args)
    (let ((syms (make-gensym-list (length fargs))))
      (translate-objects
       syms fargs types rettype
       `(,(if (stringp name-or-pointer)
              '%foreign-funcall
              '%foreign-funcall-pointer)
          ,name-or-pointer ,@(mapcan #'list ctypes syms)
          ,(canonicalize-foreign-type rettype))))))

(defun promote-varargs-type (builtin-type)
  "Default argument promotions."
  (case builtin-type
    (:float :double)
    ((:char :short) :int)
    ((:unsigned-char :unsigned-short) :unsigned-int)
    (t builtin-type)))

;;; ATM, the only difference between this macro and FOREIGN-FUNCALL is that
;;; it does argument promotion for that variadic argument. This could be useful
;;; to call an hypothetical %foreign-funcall-varargs on some hypothetical lisp
;;; on an hypothetical platform that has different calling conventions for
;;; varargs functions. :-)
(defmacro foreign-funcall-varargs (name-or-pointer fixed-args &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL(-POINTER) that translates its arguments
and does type promotion for the variadic arguments."
  (multiple-value-bind (fixed-types fixed-ctypes fixed-fargs)
      (parse-args-and-types fixed-args)
    (multiple-value-bind (varargs-types varargs-ctypes varargs-fargs rettype)
        (parse-args-and-types varargs)
      (let ((syms (make-gensym-list (+ (length fixed-fargs)
                                       (length varargs-fargs)))))
        (translate-objects
         syms (append fixed-fargs varargs-fargs)
         (append fixed-types varargs-types) rettype
         `(,(if (stringp name-or-pointer)
                '%foreign-funcall
                '%foreign-funcall-pointer)
            ,name-or-pointer
            ,@(mapcan #'list
                      (nconc fixed-ctypes
                             (mapcar #'promote-varargs-type varargs-ctypes))
                      syms)
            ,(canonicalize-foreign-type rettype)))))))

;;;# Defining Foreign Functions
;;;
;;; The DEFCFUN macro provides a declarative interface for defining
;;; Lisp functions that call foreign functions.

(defun lisp-function-name (name)
  "Return the Lisp function name for foreign function NAME."
  (etypecase name
    (list (second name))
    (string (read-from-string (substitute #\- #\_ name)))
    (symbol name)))

(defun foreign-function-name (name)
  "Return the foreign function name of NAME."
  (etypecase name
    (list (first name))
    (string name)
    (symbol (substitute #\_ #\- (string-downcase (symbol-name name))))))

;; If cffi-sys doesn't provide a defcfun-helper-forms,
;; we define one that uses %foreign-funcall.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'defcfun-helper-forms)
    (defun defcfun-helper-forms (name lisp-name rettype args types)
      (declare (ignore lisp-name))
      (values
       '()
       `(%foreign-funcall ,name ,@(mapcan #'list types args) ,rettype)))))

(defun %defcfun (lisp-name foreign-name return-type args)
  (let ((arg-names (mapcar #'car args))
        (arg-types (mapcar #'cadr args))
        (syms (make-gensym-list (length args))))
    (multiple-value-bind (prelude caller)
        (defcfun-helper-forms
          foreign-name lisp-name (canonicalize-foreign-type return-type)
          syms (mapcar #'canonicalize-foreign-type arg-types))
      `(progn
         ,prelude
         (defun ,lisp-name ,arg-names
           ,(translate-objects
             syms arg-names arg-types return-type caller))))))

(defun %defcfun-varargs (lisp-name foreign-name return-type args)
  (with-unique-names (varargs)
    (let ((arg-names (mapcar #'car args)))
      `(defmacro ,lisp-name (,@arg-names &rest ,varargs)
         `(foreign-funcall-varargs
           ,',foreign-name
           ,,`(list ,@(loop for (name type) in args
                            collect type collect name))
           ,@,varargs
           ,',return-type)))))

;;; If we find a &REST token at the end of ARGS, it's a varargs function
;;; therefore we define a lisp macro using %DEFCFUN-VARARGS instead of a
;;; lisp macro with %DEFCFUN as we would otherwise do.
(defmacro defcfun (name return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (discard-docstring args)
  (let ((lisp-name (lisp-function-name name))
        (foreign-name (foreign-function-name name)))
    (if (eq (car (last args)) '&rest)   ; probably should use STRING=
        (%defcfun-varargs lisp-name foreign-name return-type (butlast args))
        (%defcfun lisp-name foreign-name return-type args))))

;;;# Defining Callbacks

(defun inverse-translate-objects (args ignored-args types rettype call)
  "Helper function for DEFCALLBACK."
  (labels ((rec (args types)
             (cond ((null args)
                    (expand-type-to-foreign call (parse-type rettype)))
                   ;; Don't apply translations for arguments that were
                   ;; declared ignored in order to avoid warnings.
                   ((not (member (car args) ignored-args))
                    `(let ((,(car args) ,(expand-type-from-foreign
                                          (car args) (parse-type (car types)))))
                       ,(rec (cdr args) (cdr types))))
                   (t (rec (cdr args) (cdr types)))))) 
    (rec args types)))

(defun collect-ignored-args (declarations)
  (loop for declaration in declarations
        append (loop for decl in (cdr declaration)
                     when (eq (car decl) 'cl:ignore)
                     append (cdr decl))))

(defmacro defcallback (name return-type args &body body)
  (multiple-value-bind (body docstring declarations)
      (parse-body body)
    (declare (ignore docstring))
    (let ((arg-names (mapcar #'car args))
          (arg-types (mapcar #'cadr args)))
      `(progn
         (%defcallback ,name ,(canonicalize-foreign-type return-type)
             ,arg-names ,(mapcar #'canonicalize-foreign-type arg-types)
           ,@declarations
           ,(inverse-translate-objects
             arg-names (collect-ignored-args declarations) arg-types
             return-type `(block ,name ,@body)))
         ',name))))

(defun get-callback (symbol)
  (%callback symbol))

(defmacro callback (name)
  `(%callback ',name))
