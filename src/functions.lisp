;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; functions.lisp --- High-level interface to foreign functions.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
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
;;; FOREIGN-FUNCALL directly and might use something else (passed to
;;; TRANSLATE-OBJECTS as the CALL-FORM argument) instead of
;;; CFFI-SYS:%FOREIGN-FUNCALL to call the foreign-function.

(defun translate-objects (syms args types rettype call-form)
  "Helper function for FOREIGN-FUNCALL and DEFCFUN."
  (if (null args)
      (expand-from-foreign call-form (parse-type rettype))
      (expand-to-foreign-dyn
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

;;; While the options passed directly to DEFCFUN/FOREIGN-FUNCALL have
;;; precedence, we also grab its library's options, if possible.
(defun parse-function-options (options &key pointer)
  (destructuring-bind (&key (library :default libraryp) calling-convention
                            (cconv calling-convention))
      options
    (list* :calling-convention
           (or cconv
               (when libraryp
                 (let ((lib-options (foreign-library-options
                                     (get-foreign-library library))))
                   (getf lib-options :cconv
                         (getf lib-options :calling-convention))))
               :cdecl)
           ;; Don't pass the library option if we're dealing with
           ;; FOREIGN-FUNCALL-POINTER.
           (unless pointer
             (list :library library)))))

(defun foreign-funcall-form (thing options args pointerp)
  (multiple-value-bind (types ctypes fargs rettype)
      (parse-args-and-types args)
    (let ((syms (make-gensym-list (length fargs))))
      (translate-objects
       syms fargs types rettype
       `(,(if pointerp '%foreign-funcall-pointer '%foreign-funcall)
         ,thing
         (,@(mapcan #'list ctypes syms)
            ,(canonicalize-foreign-type rettype))
         ,@(parse-function-options options :pointer pointerp))))))

(defmacro foreign-funcall (name-and-options &rest args)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments."
  (let ((name (car (ensure-list name-and-options)))
        (options (cdr (ensure-list name-and-options))))
    (foreign-funcall-form name options args nil)))

(defmacro foreign-funcall-pointer (pointer options &rest args)
  (foreign-funcall-form pointer options args t))

(defun promote-varargs-type (builtin-type)
  "Default argument promotions."
  (case builtin-type
    (:float :double)
    ((:char :short) :int)
    ((:unsigned-char :unsigned-short) :unsigned-int)
    (t builtin-type)))

(defun foreign-funcall-varargs-form (thing options fixed-args varargs pointerp)
  (multiple-value-bind (fixed-types fixed-ctypes fixed-fargs)
      (parse-args-and-types fixed-args)
    (multiple-value-bind (varargs-types varargs-ctypes varargs-fargs rettype)
        (parse-args-and-types varargs)
      (let ((fixed-syms (make-gensym-list (length fixed-fargs)))
            (varargs-syms (make-gensym-list (length varargs-fargs))))
        (translate-objects
         (append fixed-syms varargs-syms)
         (append fixed-fargs varargs-fargs)
         (append fixed-types varargs-types)
         rettype
         `(,(if pointerp '%foreign-funcall-pointer '%foreign-funcall)
            ,thing
            ,(append
              (mapcan #'list
                      (nconc fixed-ctypes
                             (mapcar #'promote-varargs-type varargs-ctypes))
                      (append fixed-syms
                              (loop for sym in varargs-syms
                                    and type in varargs-ctypes
                                    if (eq type :float)
                                    collect `(float ,sym 1.0d0)
                                    else collect sym)))
              (list (canonicalize-foreign-type rettype)))
            ,@options))))))

;;; For now, the only difference between this macro and
;;; FOREIGN-FUNCALL is that it does argument promotion for that
;;; variadic argument. This could be useful to call an hypothetical
;;; %foreign-funcall-varargs on some hypothetical lisp on an
;;; hypothetical platform that has different calling conventions for
;;; varargs functions. :-)
(defmacro foreign-funcall-varargs (name-and-options fixed-args
                                   &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments
and does type promotion for the variadic arguments."
  (let ((name (car (ensure-list name-and-options)))
        (options (cdr (ensure-list name-and-options))))
    (foreign-funcall-varargs-form name options fixed-args varargs nil)))

(defmacro foreign-funcall-pointer-varargs (pointer options fixed-args
                                           &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL-POINTER that translates its
arguments and does type promotion for the variadic arguments."
  (foreign-funcall-varargs-form pointer options fixed-args varargs t))

;;;# Defining Foreign Functions
;;;
;;; The DEFCFUN macro provides a declarative interface for defining
;;; Lisp functions that call foreign functions.

;; If cffi-sys doesn't provide a defcfun-helper-forms,
;; we define one that uses %foreign-funcall.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'defcfun-helper-forms)
    (defun defcfun-helper-forms (name lisp-name rettype args types options)
      (declare (ignore lisp-name))
      (values
       '()
       `(%foreign-funcall ,name ,(append (mapcan #'list types args)
                                         (list rettype))
                          ,@options)))))

(defun %defcfun (lisp-name foreign-name return-type args options docstring)
  (let ((arg-names (mapcar #'car args))
        (arg-types (mapcar #'cadr args))
        (syms (make-gensym-list (length args))))
    (multiple-value-bind (prelude caller)
        (defcfun-helper-forms
          foreign-name lisp-name (canonicalize-foreign-type return-type)
          syms (mapcar #'canonicalize-foreign-type arg-types) options)
      `(progn
         ,prelude
         (defun ,lisp-name ,arg-names
           ,@(ensure-list docstring)
           ,(translate-objects
             syms arg-names arg-types return-type caller))))))

(defun %defcfun-varargs (lisp-name foreign-name return-type args options doc)
  (with-unique-names (varargs)
    (let ((arg-names (mapcar #'car args)))
      `(defmacro ,lisp-name (,@arg-names &rest ,varargs)
         ,@(ensure-list doc)
         `(foreign-funcall-varargs
           ,'(,foreign-name ,@options)
           ,,`(list ,@(loop for (name type) in args
                            collect `',type collect name))
           ,@,varargs
           ,',return-type)))))

;;; The following four functions take care of parsing DEFCFUN's first
;;; argument whose syntax can be one of:
;;;
;;;     1.  string
;;;     2.  symbol
;;;     3.  \( string [symbol] options* )
;;;     4.  \( symbol [string] options* )
;;;
;;; The string argument denotes the foreign function's name. The
;;; symbol argument is used to name the Lisp function. If one isn't
;;; present, its name is derived from the other. See the user
;;; documentation for an explanation of the derivation rules.

(defun lisp-name (spec &optional varp)
  (etypecase spec
    (list (if (keywordp (second spec))
              (lisp-name (first spec) varp)
              (if (symbolp (first spec))
                  (first spec)
                  (lisp-name (second spec) varp))))
    (string (intern
             (format nil (if varp "*~A*" "~A")
                     (canonicalize-symbol-name-case
                      (substitute #\- #\_ spec)))))
    (symbol spec)))

(defun foreign-name (spec &optional varp)
  (etypecase spec
    (list (if (stringp (second spec))
              (second spec)
              (foreign-name (first spec) varp)))
    (string spec)
    (symbol (let ((name (substitute #\_ #\-
                                    (string-downcase (symbol-name spec)))))
              (if varp
                  (string-trim '(#\*) name)
                  name)))))

(defun foreign-options (spec varp)
  (let ((opts (if (listp spec)
                  (if (keywordp (second spec))
                      (cdr spec)
                      (cddr spec))
                  nil)))
    (if varp
        (funcall 'parse-defcvar-options opts)
        (parse-function-options opts))))

(defun parse-name-and-options (spec &optional varp)
  (values (lisp-name spec varp)
          (foreign-name spec varp)
          (foreign-options spec varp)))

;;; If we find a &REST token at the end of ARGS, it means this is a
;;; varargs foreign function therefore we define a lisp macro using
;;; %DEFCFUN-VARARGS. Otherwise, a lisp function is defined with
;;; %DEFCFUN.
(defmacro defcfun (name-and-options return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (let ((docstring (when (stringp (car args)) (pop args))))
    (multiple-value-bind (lisp-name foreign-name options)
        (parse-name-and-options name-and-options)
      (if (eq (car (last args)) '&rest)
          (%defcfun-varargs lisp-name foreign-name return-type
                            (butlast args) options docstring)
          (%defcfun lisp-name foreign-name return-type args options
                    docstring)))))

;;;# Defining Callbacks

(defun inverse-translate-objects (args types declarations rettype call)
  `(let (,@(loop for arg in args and type in types
                 collect (list arg (expand-from-foreign
                                    arg (parse-type type)))))
     ,@declarations
     ,(expand-to-foreign call (parse-type rettype))))

(defun parse-defcallback-options (options)
  (destructuring-bind (&key (calling-convention :cdecl)
                            (cconv calling-convention))
      options
    (list :calling-convention cconv)))

(defmacro defcallback (name-and-options return-type args &body body)
  (multiple-value-bind (body declarations)
      (parse-body body :documentation t)
    (let ((arg-names (mapcar #'car args))
          (arg-types (mapcar #'cadr args))
          (name (car (ensure-list name-and-options)))
          (options (cdr (ensure-list name-and-options))))
      `(progn
         (%defcallback ,name ,(canonicalize-foreign-type return-type)
             ,arg-names ,(mapcar #'canonicalize-foreign-type arg-types)
           ,(inverse-translate-objects
             arg-names arg-types declarations return-type
             `(block ,name ,@body))
           ,@(parse-defcallback-options options))
         ',name))))

(declaim (inline get-callback))
(defun get-callback (symbol)
  (%callback symbol))

(defmacro callback (name)
  `(%callback ',name))
