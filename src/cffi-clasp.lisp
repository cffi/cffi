;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-clasp.lisp --- CLASP backend for CFFI.
;;;
;;; by Frank Gönninger, <frank.goenninger@goenninger.net>
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
  (:use #:common-lisp #:alexandria)
  (:export
   #:canonicalize-symbol-name-case
   #:foreign-pointer
   #:pointerp
   #:pointer-eq
   #:%foreign-alloc
   #:foreign-free
   #:with-foreign-pointer
   #:null-pointer
   #:null-pointer-p
   #:inc-pointer
   #:make-pointer
   #:pointer-address
   #:%mem-ref
   #:%mem-set
   #:%foreign-funcall
   #:%foreign-funcall-pointer
   #:%foreign-type-alignment
   #:%foreign-type-size
   #:%load-foreign-library
   #:%close-foreign-library
   #:native-namestring
   #:make-shareable-byte-vector
   #:with-pointer-to-vector-data
   #:%defcallback
   #:%callback
   #:%foreign-symbol-pointer))

(in-package #:cffi-sys)

;;;# Mis-features

#-long-long
(pushnew 'flat-namespace *features*)

;;;# Symbol Case

(proclaim (inline canonicalize-symbol-name-case))
(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Allocation

(proclaim (inline %foreign-alloc))
(defun %foreign-alloc (size)
  "Allocate SIZE bytes of foreign-addressable memory."
  (core:%allocate-foreign-data size))

(proclaim (inline %foreign-free))
(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC."
  (core:%free-foreign-data ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let* ((,size-var ,size)
          (,var (%foreign-alloc ,size-var)))
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;;# Misc. Pointer Operations

(deftype foreign-pointer ()
  'core:foreign-data)

(proclaim (inline null-pointer))
(defun null-pointer ()
  "Construct and return a null pointer."
  (core:%make-null-pointer))

(proclaim (inline inc-pointer))
(defun inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (core:%inc-pointer ptr offset))

(proclaim (inline pointerp))
(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (typep ptr 'core:foreign-data))

(proclaim (inline pointer-eq))
(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (core:eql ptr1 ptr2))

(proclaim (inline make-pointer))
(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (core:%make-pointer address))

(proclaim (inline pointer-address))
(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (core:%pointer-address ptr))

;;;# Shareable Vectors
;;;
;;; This interface is very experimental.  WITH-POINTER-TO-VECTOR-DATA
;;; should be defined to perform a copy-in/copy-out if the Lisp
;;; implementation can't do this.

(proclaim (inline make-shareable-byte-vector))
(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes that can passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

;; frgo, 2016-07-02: TODO: Implemenent!
;; (defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
;;   "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
;;   `(let ((,ptr-var (si:make-foreign-data-from-array ,vector)))
;;      ,@body))

(proclaim (inline %foreign-type-size))
(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (core:%foreign-type-size type-keyword))

(proclaim (inline %foreign-type-alignment))
(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type."
  (core:%foreign-type-alignment type-keyword))

;;;# Dereferencing

(proclaim (inline %mem-ref))
(defun %mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (core:%mem-ref ptr type offset))

(proclaim (inline %mem-set))
(defun %mem-set (value ptr type &optional (offset 0))
  "Set an object of TYPE at OFFSET bytes from PTR."
  (core:%mem-set ptr type offset value))

(proclaim (inline %foreign-funcall))
(defmacro %foreign-funcall (name args &key library convention)
  "Call a foreign function."
  (declare (ignore library convention))
  `(core:%foreign-funcall ,name ,@args))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Funcall a pointer to a foreign function."
  (declare (ignore convention))
  `(core:%foreign-funcall-pointer ,ptr ,@args))

;;;# Foreign Libraries

(proclaim (inline %load-foreign-library))
(defun %load-foreign-library (name path)
  "Load a foreign library."
  (declare (ignore name))
  (core:%load-foreign-library path))

(proclaim (inline %close-foreign-library))
(defun %close-foreign-library (handle)
  "Close a foreign library."
  (core:%close-foreign-library handle))

(proclaim (inline %foreign-symbol-pointer))
(defun %foreign-symbol-pointer (name library)
  "Returns a pointer to a foreign symbol NAME."
  (declare (ignorable library))
  (core:%foreign-symbol-pointer name library))

(proclaim (inline native-namestring))
(defun native-namestring (pathname)
  (namestring pathname))

#| frgo, 2016-07-02: TODO: Implement callback handling!

;;;# Callbacks

;;; Create a package to contain the symbols for callback functions.
;;; We want to redefine callbacks with the same symbol so the internal
;;; data structures are reused.

(defpackage #:cffi-callbacks
  (:use))

(defvar *callbacks* (cl:make-hash-table))

;;; Intern a symbol in the CFFI-CALLBACKS package used to name the
;;; internal callback for NAME.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-callback (name)
    (intern (format nil "~A::~A"
                    (if-let (package (symbol-package name))
                      (package-name package)
                      "#")
                    (symbol-name name))
            '#:cffi-callbacks)))

(defmacro %defcallback (name rettype arg-names arg-types body
                        &key convention)
  (declare (ignore convention))
  (let ((cb-name (intern-callback name))
        (cb-type #.(if (> ext:+ecl-version-number+ 160102)
                       :default :cdecl)))
    `(progn
       (ffi:defcallback (,cb-name ,cb-type)
           ,(cffi-type->ecl-type rettype)
           ,(mapcar #'list arg-names
                    (mapcar #'cffi-type->ecl-type arg-types))
         ,body)
       (setf (gethash ',name *callbacks*) ',cb-name))))

(defun %callback (name)
  (multiple-value-bind (symbol winp)
      (gethash name *callbacks*)
    (unless winp
      (error "Undefined callback: ~S" name))
    (ffi:callback symbol)))

|# ;; END OF TODO IMPLEMENT CALLBACK HANDLING
