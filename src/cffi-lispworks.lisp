;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-lispworks.lisp --- Lispworks CFFI-SYS implementation.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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

;;;# Administrivia

(defpackage #:cffi-sys
  (:use #:cl #:cffi-utils)
  (:export
   #:canonicalize-symbol-name-case
   #:pointerp
   #:pointer-eq
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring
   #:%mem-ref
   #:%mem-set
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:foreign-symbol-pointer
   #:defcfun-helper-forms
   #:%defcallback
   #:%callback
   #:finalize
   #:cancel-finalization))

(in-package #:cffi-sys)

;;;# Features

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc (lambda (feature) (pushnew feature *features*))
        '(;; Backend mis-features.
          cffi-features:no-long-long
          ;; OS/CPU features.
          #+darwin  cffi-features:darwin
          #+unix    cffi-features:unix
          #+win32   cffi-features:windows
          #+harp::pc386   cffi-features:x86
          #+harp::powerpc cffi-features:ppc32
          )))

;;; Symbol case.

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Basic Pointer Operations
(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (fli:pointerp ptr))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (fli:pointer-eq ptr1 ptr2))

;; We use FLI:MAKE-POINTER here instead of FLI:*NULL-POINTER* since old
;; versions of Lispworks don't seem to have it.
(defun null-pointer ()
  "Return a null foreign pointer."
  (fli:make-pointer :address 0 :type :void))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (fli:null-pointer-p ptr))

;; FLI:INCF-POINTER won't work on FLI pointers to :void so we
;; increment "manually."
(defun inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (fli:make-pointer :type :void :address (+ (fli:pointer-address ptr) offset)))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (fli:make-pointer :type :void :address address))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (fli:pointer-address ptr))

;;;# Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of memory and return a pointer."
  (fli:allocate-foreign-object :type :byte :nelems size))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC."
  (fli:free-foreign-object ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  Both the
pointer in VAR and the memory it points to have dynamic extent and may
be stack allocated if supported by the implementation."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(fli:with-dynamic-foreign-objects ()
     (let* ((,size-var ,size)
            (,var (fli:alloca :type :byte :nelems ,size-var)))
       ,@body)))

;;;# Shareable Vectors

(defun make-shareable-byte-vector (size)
  "Create a shareable byte vector."
  (sys:in-static-area
    (make-array size :element-type '(unsigned-byte 8))))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a pointer at the data in VECTOR."
  `(fli:with-dynamic-lisp-array-pointer (,ptr-var ,vector)
     ,@body))

;;;# Dereferencing

(defun convert-foreign-type (cffi-type)
  "Convert a CFFI type keyword to an FLI type."
  (ecase cffi-type
    (:char            :byte)
    (:unsigned-char   '(:unsigned :byte))
    (:short           :short)
    (:unsigned-short  '(:unsigned :short))
    (:int             :int)
    (:unsigned-int    '(:unsigned :int))
    (:long            :long)
    (:unsigned-long   '(:unsigned :long))
    (:float           :float)
    (:double          :double)
    (:pointer         :pointer)
    (:void            :void)))

;;; Convert a CFFI type keyword to a symbol suitable for passing to
;;; FLI:FOREIGN-TYPED-AREF.
#+#.(cl:if (cl:find-symbol "FOREIGN-TYPED-AREF" "FLI") '(and) '(or))
(defun convert-foreign-typed-aref-type (cffi-type)
  (ecase cffi-type
    ((:char :short :int :long)
     `(signed-byte ,(* 8 (%foreign-type-size cffi-type))))
    ((:unsigned-char :unsigned-short :unsigned-int :unsigned-long)
     `(unsigned-byte ,(* 8 (%foreign-type-size cffi-type))))
    (:float 'single-float)
    (:double 'double-float)))

(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of type TYPE OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-pointer ptr offset)))
  (fli:dereference ptr :type (convert-foreign-type type)))

;;; In LispWorks versions where FLI:FOREIGN-TYPED-AREF is fbound, use
;;; it instead of FLI:DEREFERENCE in the optimizer for %MEM-REF.
#+#.(cl:if (cl:find-symbol "FOREIGN-TYPED-AREF" "FLI") '(and) '(or))
(define-compiler-macro %mem-ref (&whole form ptr type &optional (off 0))
  (if (constantp type)
      (let ((type (eval type)))
        (if (eql type :pointer)
            (let ((fli-type (convert-foreign-type type))
                  (ptr-form (if (eql off 0) ptr `(inc-pointer ,ptr ,off))))
              `(fli:dereference ,ptr-form :type ',fli-type))
            (let ((lisp-type (convert-foreign-typed-aref-type type))) 
              `(locally
                   (declare (optimize (speed 3) (safety 0)))
                 (fli:foreign-typed-aref ',lisp-type ,ptr (the fixnum ,off))))))
      form))

;;; Open-code the call to FLI:DEREFERENCE when TYPE is constant at
;;; macroexpansion time, when FLI:FOREIGN-TYPED-AREF is not available.
#-#.(cl:if (cl:find-symbol "FOREIGN-TYPED-AREF" "FLI") '(and) '(or))
(define-compiler-macro %mem-ref (&whole form ptr type &optional (off 0))
  (if (constantp type)
      (let ((ptr-form (if (eql off 0) ptr `(inc-pointer ,ptr ,off)))
            (type (convert-foreign-type (eval type))))
        `(fli:dereference ,ptr-form :type ',type))
      form))

(defun %mem-set (value ptr type &optional (offset 0))
  "Set the object of TYPE at OFFSET bytes from PTR."
  (unless (zerop offset)
    (setf ptr (inc-pointer ptr offset)))
  (setf (fli:dereference ptr :type (convert-foreign-type type)) value))

;;; In LispWorks versions where FLI:FOREIGN-TYPED-AREF is fbound, use
;;; it instead of FLI:DEREFERENCE in the optimizer for %MEM-SET.
#+#.(cl:if (cl:find-symbol "FOREIGN-TYPED-AREF" "FLI") '(and) '(or))
(define-compiler-macro %mem-set (&whole form val ptr type &optional (off 0))
  (if (constantp type)
      (once-only (val)
        (let ((type (eval type)))
          (if (eql type :pointer)
              (let ((fli-type (convert-foreign-type type))
                    (ptr-form (if (eql off 0) ptr `(inc-pointer ,ptr ,off))))
                `(setf (fli:dereference ,ptr-form :type ',fli-type) ,val))
              (let ((lisp-type (convert-foreign-typed-aref-type type)))
                `(locally
                     (declare (optimize (speed 3) (safety 0)))
                   (setf (fli:foreign-typed-aref ',lisp-type ,ptr
                                                 (the fixnum ,off))
                         ,val))))))
      form))

;;; Open-code the call to (SETF FLI:DEREFERENCE) when TYPE is constant
;;; at macroexpansion time.
#-#.(cl:if (cl:find-symbol "FOREIGN-TYPED-AREF" "FLI") '(and) '(or))
(define-compiler-macro %mem-set (&whole form val ptr type &optional (off 0))
  (if (constantp type)
      (once-only (val)
        (let ((ptr-form (if (eql off 0) ptr `(inc-pointer ,ptr ,off)))
              (type (convert-foreign-type (eval type))))
          `(setf (fli:dereference ,ptr-form :type ',type) ,val)))
      form))

;;;# Foreign Type Operations

(defun %foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (fli:size-of (convert-foreign-type type)))

(defun %foreign-type-alignment (type)
  "Return the structure alignment in bytes of foreign type."
  #+(and darwin harp::powerpc)
  (when (eq type :double)
    (return-from %foreign-type-alignment 8))
  ;; Override not necessary for the remaining types...
  (fli:align-of (convert-foreign-type type)))

;;;# Calling Foreign Functions

(defvar *foreign-funcallable-cache* (make-hash-table :test 'equal)
  "Caches foreign funcallables created by %FOREIGN-FUNCALL or
%FOREIGN-FUNCALL-POINTER.  We only need to have one per each
signature.")

(defun foreign-funcall-type-and-args (args)
  "Returns a list of types, list of args and return type."
  (let ((return-type :void))
    (loop for (type arg) on args by #'cddr
          if arg collect (convert-foreign-type type) into types
          and collect arg into fargs
          else do (setf return-type (convert-foreign-type type))
          finally (return (values types fargs return-type)))))

(defun create-foreign-funcallable (types rettype)
  "Creates a foreign funcallable for the signature TYPES -> RETTYPE."
  (format t "~&Creating foreign funcallable for signature ~S -> ~S~%"
          types rettype)
  ;; yes, ugly, this most likely wants to be a top-level form...
  (let ((internal-name (gensym)))
    (funcall
     (compile nil
              `(lambda ()
                 (fli:define-foreign-funcallable ,internal-name
                     ,(loop for type in types
                            collect (list (gensym) type))
                   :result-type ,rettype
                   :language :ansi-c
                   ;; avoid warning about cdecl not being supported on mac
                   #-mac ,@'(:calling-convention :cdecl)))))
    internal-name))

(defun get-foreign-funcallable (types rettype)
  "Returns a foreign funcallable for the signature TYPES -> RETTYPE -
either from the cache or newly created."
  (let ((signature (cons rettype types)))
    (or (gethash signature *foreign-funcallable-cache*)
        ;; (SETF GETHASH) is supposed to be thread-safe
        (setf (gethash signature *foreign-funcallable-cache*)
              (create-foreign-funcallable types rettype)))))

(defmacro %%foreign-funcall (foreign-function &rest args)
  "Does the actual work for %FOREIGN-FUNCALL-POINTER and %FOREIGN-FUNCALL.
Checks if a foreign funcallable which fits ARGS already exists and creates
and caches it if necessary.  Finally calls it."
  (multiple-value-bind (types fargs rettype)
      (foreign-funcall-type-and-args args)
    `(funcall (load-time-value (get-foreign-funcallable ',types ',rettype))
              ,foreign-function ,@fargs)))

(defmacro %foreign-funcall (name &rest args)
  "Calls a foreign function named NAME passing arguments ARGS."
  `(%%foreign-funcall (fli:make-pointer :symbol-name ,name) ,@args))

(defmacro %foreign-funcall-pointer (ptr &rest args)
  "Calls a foreign function pointed at by PTR passing arguments ARGS."
  `(%%foreign-funcall ,ptr ,@args))

(defun defcfun-helper-forms (name lisp-name rettype args types)
  "Return 2 values for DEFCFUN. A prelude form and a caller form."
  (let ((ff-name (intern (format nil "%cffi-foreign-function/~A"  lisp-name))))
    (values
     `(fli:define-foreign-function (,ff-name ,name :source)
          ,(mapcar (lambda (ty) (list (gensym) (convert-foreign-type ty)))
                   types)
        :result-type ,(convert-foreign-type rettype)
        :language :ansi-c
        ;; avoid warning about cdecl not being supported on mac platforms
        #-mac ,@'(:calling-convention :cdecl))
     `(,ff-name ,@args))))

;;;# Callbacks

(defvar *callbacks* (make-hash-table))

;;; Create a package to contain the symbols for callback functions.  We
;;; want to redefine callbacks with the same symbol so the internal data
;;; structures are reused.
(defpackage #:cffi-callbacks
  (:use))

;;; Intern a symbol in the CFFI-CALLBACKS package used to name the internal
;;; callback for NAME.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-callback (name)
    (intern (format nil "~A::~A" (package-name (symbol-package name))
                    (symbol-name name))
            '#:cffi-callbacks)))

(defmacro %defcallback (name rettype arg-names arg-types &body body)
  (let ((cb-name (intern-callback name)))
    `(progn
       (fli:define-foreign-callable
           (,cb-name :encode :lisp
                     :result-type ,(convert-foreign-type rettype)
                     :calling-convention :cdecl
                     :language :ansi-c
                     :no-check nil)
           ,(mapcar (lambda (sym type)
                      (list sym (convert-foreign-type type)))
                    arg-names arg-types)
         ,@body)
       (setf (gethash ',name *callbacks*) ',cb-name))))

(defun %callback (name)
  (multiple-value-bind (symbol winp)
      (gethash name *callbacks*)
    (unless winp
      (error "Undefined callback: ~S" name))
    (fli:make-pointer :symbol-name symbol :module :callbacks)))

;;;# Loading Foreign Libraries

(defun %load-foreign-library (name)
  "Load the foreign library NAME."
  (fli:register-module name :connection-style :immediate))

(defun %close-foreign-library (name)
  "Close the foreign library NAME."
  (fli:disconnect-module name :remove t))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Foreign Globals

(defun foreign-symbol-pointer (name)
  "Returns a pointer to a foreign symbol NAME."
  (prog1 (ignore-errors (fli:make-pointer :symbol-name name :type :void))))

;;;# Finalizers

(defvar *finalizers* (make-hash-table :test 'eq :weak-kind :key)
  "Weak hashtable that holds registered finalizers.")

(hcl:add-special-free-action 'free-action)

(defun free-action (object)
  (let ((finalizers (gethash object *finalizers*)))
    (unless (null finalizers)
      (mapc #'funcall finalizers))))

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
finalizers. FUNCTION should take no arguments. Returns OBJECT.

For portability reasons, FUNCTION should not attempt to look at
OBJECT by closing over it because, in some lisps, OBJECT will
already have been garbage collected and is therefore not
accessible when FUNCTION is invoked."
  (push function (gethash object *finalizers*))
  (hcl:flag-special-free-action object)
  object)

(defun cancel-finalization (object)
  "Cancels all of OBJECT's finalizers, if any."
  (remhash object *finalizers*)
  (hcl:flag-not-special-free-action object))
