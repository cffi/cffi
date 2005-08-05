;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; types.lisp --- User-defined CFFI types.
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

;;;# Built-In Types

(define-built-in-foreign-type :char)
(define-built-in-foreign-type :unsigned-char)
(define-built-in-foreign-type :short)
(define-built-in-foreign-type :unsigned-short)
(define-built-in-foreign-type :int)
(define-built-in-foreign-type :unsigned-int)
(define-built-in-foreign-type :long)
(define-built-in-foreign-type :unsigned-long)
(define-built-in-foreign-type :float)
(define-built-in-foreign-type :double)
(define-built-in-foreign-type :pointer)
(define-built-in-foreign-type :void)

;;;# Dereferencing Foreign Pointers

(defun mem-ref (ptr type &optional (offset 0))
  "Return the value of TYPE at OFFSET from PTR."
  (%mem-ref ptr (canonicalize-foreign-type type) offset))

(define-compiler-macro mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code MEM-REF when TYPE is constant."
  (if (constantp type)
      `(%mem-ref ,ptr ,(canonicalize-foreign-type (eval type)) ,offset)
      form))

(defun mem-set (value ptr type &optional (offset 0))
  "Set the value of TYPE at OFFSET from PTR to VALUE."
  (setf (%mem-ref ptr (canonicalize-foreign-type type) offset) value))

(define-setf-expander mem-ref (ptr type &optional (offset 0))
  "SETF expander for MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on MEM-SET to be able
to open-code (SETF MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr)
    (declare (ignore setter newval))
    (let ((store (gensym)))
      (values
       dummies
       vals
       `(,store)
       `(progn
          (mem-set ,store ,getter ,type ,offset)
          ,store)
       `(mem-ref ,getter)))))

(define-compiler-macro mem-set
    (&whole form value ptr type &optional (offset 0))
  "Compiler macro to open-code (SETF MEM-REF) when type is constant."
  (if (constantp type)
      `(setf (%mem-ref ,ptr ,(canonicalize-foreign-type (eval type))
                       ,offset) ,value)
      form))

;;;# Dereferencing Foreign Arrays

;;; TODO: FOREIGN-AREF needs its own SETF expander and a compiler
;;; macro to optimize array references when TYPE is constant.
(defun foreign-aref (ptr type &optional (index 0))
  "Like MEM-REF except for accessing arrays."
  (mem-ref ptr type (* index (foreign-type-size type))))

(defun (setf foreign-aref) (value ptr type &optional (index 0))
  "Set the INDEXth element of an array of TYPE at PTR to VALUE."
  (setf (mem-ref ptr type (* index (foreign-type-size type))) value))

;;;# Foreign Structures

;;;## Foreign Structure Slots

(defgeneric foreign-struct-slot-address (ptr slot)
  (:documentation
   "Get the address of SLOT relative to PTR."))

(defgeneric foreign-struct-slot-address-form (ptr slot)
  (:documentation
   "Return a form to get the address of SLOT in PTR."))

(defgeneric foreign-struct-slot-value (ptr slot)
  (:documentation
   "Return the value of SLOT in structure PTR."))

(defgeneric (setf foreign-struct-slot-value) (value ptr slot)
  (:documentation
   "Set the value of a SLOT in structure PTR."))

(defgeneric foreign-struct-slot-value-form (ptr slot)
  (:documentation
   "Return a form to get the value of SLOT in struct PTR."))

(defgeneric foreign-struct-slot-set-form (value ptr slot)
  (:documentation
   "Return a form to set the value of SLOT in struct PTR."))

(defclass foreign-struct-slot ()
  ((name   :initarg :name   :reader   slot-name)
   (offset :initarg :offset :accessor slot-offset)
   (type   :initarg :type   :accessor slot-type))
  (:documentation "Base class for simple and aggregate slots."))

(defmethod foreign-struct-slot-address (ptr (slot foreign-struct-slot))
  "Return the address of SLOT relative to PTR."
  (inc-ptr ptr (slot-offset slot)))

(defmethod foreign-struct-slot-address-form (ptr (slot foreign-struct-slot))
  "Return a form to get the address of SLOT relative to PTR."
  (let ((offset (slot-offset slot)))
    (if (zerop offset)
        ptr
        `(inc-ptr ,ptr ,offset))))

;;;### Simple Slots

(defclass simple-struct-slot (foreign-struct-slot)
  ()
  (:documentation "Non-aggregate structure slots."))

(defmethod foreign-struct-slot-value (ptr (slot simple-struct-slot))
  "Return the value of a simple SLOT from a struct at PTR."
  (mem-ref ptr (slot-type slot) (slot-offset slot)))

(defmethod foreign-struct-slot-value-form (ptr (slot simple-struct-slot))
  "Return a form to get the value of a slot from PTR."
  `(mem-ref ,ptr ,(slot-type slot) ,(slot-offset slot)))

(defmethod (setf foreign-struct-slot-value) (value ptr (slot simple-struct-slot))
  "Set the value of a simple SLOT to VALUE in PTR."
  (setf (mem-ref ptr (slot-type slot) (slot-offset slot)) value))

(defmethod foreign-struct-slot-set-form (value ptr (slot simple-struct-slot))
  "Return a form to set the value of a simple structure slot."
  `(setf (mem-ref ,ptr ,(slot-type slot) ,(slot-offset slot)) ,value))

;;;### Aggregate Slots

(defclass aggregate-struct-slot (foreign-struct-slot)
  ((count :initarg :count :accessor slot-count))
  (:documentation "Aggregate structure slots."))

;;; A case could be made for just returning an error here instead of
;;; this rather DWIM-ish behavior to return the address.  It would
;;; complicate being able to chain together slot names when accessing
;;; slot values in nested structures though.
(defmethod foreign-struct-slot-value (ptr (slot aggregate-struct-slot))
  "Return a pointer to SLOT relative to PTR."
  (foreign-struct-slot-address ptr slot))

(defmethod foreign-struct-slot-value-form (ptr (slot aggregate-struct-slot))
  "Return a form to get the value of SLOT relative to PTR."
  (foreign-struct-slot-address-form ptr slot))

;;; This is definitely an error though.  Eventually, we could define a
;;; new type of type translator that can convert certain aggregate
;;; types, notably C strings or arrays of integers.  For now, just error.
(defmethod (setf foreign-struct-slot-value) (value ptr (slot aggregate-struct-slot))
  "Signal an error; setting aggregate slot values is forbidden."
  (declare (ignore value ptr))
  (error "Cannot set value of aggregate slot ~A." slot))

(defmethod foreign-struct-slot-set-form (value ptr (slot aggregate-struct-slot))
  "Signal an error; setting aggregate slot values is forbidden."
  (declare (ignore value ptr))
  (error "Cannot set value of aggregate slot ~A." slot))

;;;## Defining Foreign Structures

(defun make-struct-slot (name offset type count)
  "Make the appropriate type of structure slot."
  ;; If TYPE is an aggregate type or COUNT is >1, create an
  ;; AGGREGATE-STRUCT-SLOT, otherwise a SIMPLE-STRUCT-SLOT.
  (if (or (> count 1) (aggregatep (find-foreign-type type)))
      (make-instance 'aggregate-struct-slot :offset offset :type type
                     :name name :count count)
      (make-instance 'simple-struct-slot :offset offset :type type
                     :name name)))

(defun adjust-for-alignment (type offset)
  "Return OFFSET aligned properly for TYPE."
  (let* ((align (foreign-type-alignment type))
         (rem (mod offset align)))
    (if (zerop rem)
        offset
        (+ offset (- align rem)))))

(defun notice-foreign-struct-definition (name slots)
  "Parse and install a foreign structure definition."
  (let ((struct (make-instance 'foreign-struct-type :name name))
        (offset 0))
    (dolist (slotdef slots)
      (destructuring-bind (slotname type &optional (count 1)) slotdef
        (setf offset (adjust-for-alignment type offset))
        (let ((slot (make-struct-slot slotname offset type count)))
          (setf (gethash slotname (slots struct)) slot))
        (incf offset (* count (foreign-type-size type)))))
    (setf (size struct) offset)
    (setf (find-foreign-type name) struct)))

(defmacro defcstruct (name &body fields)
  "Define the layout of a foreign structure."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-struct-definition ',name ',fields)))

;;;## Accessing Foreign Structure Slots

(defun get-slot-info (type slot-name)
  "Return the slot info for SLOT-NAME or raise an error."
  (let ((struct (find-foreign-type type)))
    (unless struct
      (error "Undefined foreign type ~A." type))
    (let ((info (gethash slot-name (slots struct))))
      (unless info
        (error "Undefined slot ~A in foreign type ~A." slot-name type))
      info)))

(defun foreign-slot-address (ptr type slot-name)
  "Return the address of SLOT-NAME in the structure at PTR."
  (foreign-struct-slot-address ptr (get-slot-info type slot-name)))

;; This is the slow interface to getting the fields of foreign slots.
;; Eventually there will be a compiler macro that optimizes this when
;; the structure type and slot name are known at compile time.
(defun foreign-slot-value (ptr type slot-name)
  "Return the value of SLOT-NAME in the foreign structure at PTR."
  (foreign-struct-slot-value ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-value (&whole form ptr type slot-name)
  "Optimizer for FOREIGN-SLOT-VALUE when TYPE is constant."
  (declare (ignore ptr type slot-name))
  form
  #+nil (if (and (constantp type) (constantp slot-name))
            (destructuring-bind (&key offset type)
                (get-slot-info (eval type) (eval slot-name))
              `(mem-ref ,ptr ',type ,offset))
            form))

(define-setf-expander foreign-slot-value (ptr type slot-name)
  "SETF expander for MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on MEM-SET to be able
to open-code (SETF MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr)
    (declare (ignore setter newval))
    (let ((store (gensym)))
      (values
       dummies
       vals
       `(,store)
       `(progn
          (foreign-slot-set ,store ,getter ,type ,slot-name)
          ,store)
       `(foreign-slot-value ,getter)))))

(defun foreign-slot-set (value ptr type slot-name)
  "Set the value of SLOT-NAME in a foreign structure."
  (setf (foreign-struct-slot-value ptr (get-slot-info type slot-name)) value))

(define-compiler-macro foreign-slot-set
    (&whole form value ptr type slot-name)
  "Optimizer when TYPE and SLOT-NAME are constant."
  (declare (ignore value ptr type slot-name))
  form
  #+nil
  (if (and (constantp type) (constantp slot-name))
      (destructuring-bind (&key offset type)
          (get-slot-info (eval type) (eval slot-name))
        `(setf (mem-ref ,ptr ,type ,offset) ,value))
      form))

(defmacro with-foreign-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE.  Similar to WITH-SLOTS."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop for var in vars
                  collect `(,var (foreign-slot-value ,ptr-var ',type ',var)))
         ,@body))))

;;;# Foreign Unions
;;;
;;; A union is a FOREIGN-STRUCT-TYPE in which all slots have an offset
;;; of zero.

(defun notice-foreign-union-definition (name slots)
  "Parse and install a foreign union definition."
  (let ((struct (make-instance 'foreign-struct-type :name name))
        (max-size 0))
    (dolist (slotdef slots)
      (destructuring-bind (slotname type &optional (count 1)) slotdef
        (let ((slot (make-struct-slot slotname 0 type count))
              (size (* count (foreign-type-size type))))
          (setf (gethash slotname (slots struct)) slot)
          (when (> size max-size)
            (setf max-size size)))))
    (setf (size struct) max-size)
    (setf (find-foreign-type name) struct)))

(defmacro defcunion (name &body fields)
  "Define the layout of a foreign union."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-union-definition ',name ',fields)))

;;;# Operations on Types

(defmethod foreign-type-alignment ((type symbol))
  "Return the alignment in bytes of a foreign type."
  (%foreign-type-alignment (canonicalize-foreign-type type)))

(defmethod foreign-type-size ((type symbol))
  "Return the size in bytes of a foreign type."
  (foreign-type-size (find-foreign-type type)))

(defun foreign-object-alloc (type &optional (count 1))
  "Allocate COUNT foreign objects of type TYPE.
The object must be freed with FOREIGN-TYPE-FREE.  Does not yet
support allocating structures, but it should."
  (foreign-alloc (* count (foreign-type-size type))))

(defun foreign-object-free (ptr)
  "Free an object allocated by FOREIGN-OBJECT-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-object ((var type &optional (count 1)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated.  TYPE
is evaluated at macroexpansion time but COUNT is evaluated at run
time."
  ;; If COUNT is constant, multiply it by the size now.
  (if (constantp count)
      `(with-foreign-ptr (,var ,(* count (foreign-type-size type)))
         ,@body)
      `(with-foreign-ptr (,var (* ,count ,(foreign-type-size type)))
         ,@body)))

;;;# Type Translators
;;;
;;; Type translators are functions that return multiple forms
;;; containing code to automatically convert foreign values to Lisp
;;; objects when calling foreign functions.

(defvar *type-translators* (make-hash-table :test 'equal)
  "Hash table of defined type translators.")

(defun find-type-translator (type direction)
  "Return the type translator function for TYPE in DIRECTION."
  (gethash (list type direction) *type-translators*))

(defun (setf find-type-translator) (func type direction)
  "Set the type translator function for TYPE in DIRECTION."
  (setf (gethash (list type direction) *type-translators*) func))

(defmacro define-type-translation (type c-type &body args)
  "Define type translation for TYPE. If C-TYPE is not nil, TYPE is setup
as an alias for C-TYPE via DEFCTYPE. ARGS consist of optional keyword
arguments :TO-C-ARG, :TO-C and :FROM-C. These should be functions accepting
a <value> argument in the case of :TO-C and :FROM-C and an addional 2
arguments <body> and <var> in the case of :TO-C-ARG."
  (when (typep (car args) 'string) ; discard docstring
    (setf args (cdr args)))
  (destructuring-bind (&key to-c-arg to-c from-c) args
    `(progn
       ,(when c-type `(defctype ,type ,c-type))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@(loop for direction in '(:to-c-arg :to-c :from-c)
                 and func in (list to-c-arg to-c from-c)
                 when func collect
                 `(setf (find-type-translator ',type ,direction) ,func))))))


;;; TODO: This should probably chase the type alias chain and call all
;;; the type translators in most-specific-last order, so users can
;;; define typedefs for STRING that still get translated.
(defmacro with-object-translated ((var value type direction) &body body)
  "Bind VAR to VALUE translated according to TYPE and DIRECTION in BODY.
If there is no translator for TYPE, VAR is bound to VALUE unmodified."
  (let ((func (find-type-translator type direction)))
    ;; If there is no TO-C-ARG translator, but a TO-C translator
    ;; is available, make this a TO-C translation.
    (when (and (eq direction :to-c-arg) (not func))
      (let ((to-c-func (find-type-translator type :to-c)))
        (when to-c-func
          (setq func to-c-func
                direction :to-c))))
    ;; Translation.
    (if func
        (case direction
          (:to-c-arg (funcall func var value body))
          (t `(let ((,var ,(funcall func value)))
                ,@body)))
        `(let ((,var ,value))
           ,@body))))
