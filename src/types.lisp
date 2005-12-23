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

#-cffi/no-long-long
(progn
  (define-built-in-foreign-type :long-long)
  (define-built-in-foreign-type :unsigned-long-long))

;;;# Type Translators
;;;
;;; Type translation is now done with generic functions at runtime.
;;;
;;; These generic functions can be specialized on either the foreign
;;; type class, or the class _and_ the foreign type name.  This is
;;; useful because we have two different types of translators:
;;;
;;; a. We want to translate _all_ values of a subclass of FOREIGN-TYPE
;;;    the same way, such as FOREIGN-ENUM.
;;;
;;; b. We want to translate a _specific instance_ of a subclass of
;;;    FOREIGN-TYPE in its own way, such as the FOREIGN-TYPEDEF instance
;;;    for the :STRING type.
;;;
;;; Note that, due to the order in which applicable methods are
;;; computed, if you are EQL-specializing the type _name_, you will
;;; also need to be sure to specialize the type _class_, otherwise you
;;; may be suprised by the method that ends up being invoked!
;;;
;;; In macros that look up type specifiers at macroexpansion-time, we
;;; avoid calling type translators for built-in types.  This
;;; optimization could be made better by wrapping the method
;;; definitions for translators to set some kind of flag in the
;;; FOREIGN-TYPE instance that specifies whether a translator is
;;; defined or not, letting us avoid translation for simple typedefs.
;;;
;;; Whether this is worth optimizing just to avoid one GF call is up
;;; for debate.
;;;
;;; TODO: Define a higher-level interface on these methods?  We can't
;;; really be compatible with the old DEFINE-TYPE-TRANSLATOR because
;;; we aren't returning list structure anymore.

;;; Convert the Lisp object VALUE to a C object having the foreign
;;; type described by TYPE-CLASS and TYPE-NAME.  The primary value is
;;; the C object to pass to the foreign function.  The second value
;;; will be passed to FREE-TRANSLATED-OBJECT and defaults to NIL.
;;; This is used for communication between the two functions.
(defgeneric translate-to-foreign (value type-class type-name)
  (:argument-precedence-order type-name type-class value))

;;; Convert the C object VALUE having the foreign type described by
;;; TYPE-CLASS and TYPE-NAME to a Lisp object and return it.
(defgeneric translate-from-foreign (value type-class type-name)
  (:argument-precedence-order type-name type-class value))

;;; Free a C object VALUE having the foreign type described by
;;; TYPE-CLASS and TYPE-NAME, allocated by TRANSLATE-TO-FOREIGN.
;;; ALLOC-PARAM contains the second value returned by
;;; TRANSLATE-TO-FOREIGN (or nil if no value was supplied).
(defgeneric free-translated-object (value type-class type-name alloc-param)
  (:argument-precedence-order type-name type-class value alloc-param))

;;;## Default Translations
;;;
;;; Default implementations of translation methods that do nothing.
;;;
;;; The results are undefined if these methods are redefined.

(defmethod translate-to-foreign (value class name)
  (declare (ignore class name))
  value)

(defmethod translate-from-foreign (value class name)
  (declare (ignore class name))
  value)

(defmethod free-translated-object (value class name param)
  (declare (ignore value class name param)))

;;;## Translations for Built-In Types
;;;
;;; By definition, there is nothing to do here, so translators for the
;;; built-in types simply return the value untouched.  This case is
;;; going to be worth optimizing when the type is known at
;;; compile-time to avoid the useless generic function dispatch.
;;;
;;; The user may not define new methods on built-in foreign types, as
;;; the optimizer will ignore them.

;;;## Translations for Typedefs
;;;
;;; We follow the chain of typedefs and apply all translations in a
;;; useful order. The actual translations are in the methods that
;;; specialize on the first arg (type (eql <name-of-typedef>)).

(defmethod translate-to-foreign (value (class foreign-typedef) name)
  (declare (ignore name))
  (let ((actual-type (actual-type class)))
    (translate-to-foreign value actual-type (name actual-type))))

(defmethod translate-from-foreign (value (class foreign-typedef) name)
  (declare (ignore name))
  (let ((actual-type (actual-type class)))
    (translate-from-foreign value actual-type (name actual-type))))

(defmethod free-translated-object (value (class foreign-typedef) name param)
  (declare (ignore name))
  (let ((actual-type (actual-type class)))
    (free-translated-object value actual-type (name actual-type) param)))

;;;## Macroexpansion Time Translation
;;;
;;; In this new implementation of type translation,
;;; WITH-OBJECT-TRANSLATED should only be used when VAR should be
;;; translated and bound to VALUE, and has dynamic extent during BODY.
;;; There is no need to use this macro when converting values from C
;;; objects---just call TRANSLATE-FROM-FOREIGN directly.
;;;
;;; If TYPE-SPEC refers to a built-in type, it will not be translated.

(defmacro with-object-translated ((var value type-spec) &body body)
  (let ((type (parse-type type-spec))
        (param (gensym "PARAM-")))
    (if (typep type 'foreign-built-in-type)
        `(let ((,var ,value))
          ,@body)
        `(multiple-value-bind (,var ,param)
          (translate-to-foreign ,value ,type ',(name type))
          (unwind-protect
               (progn ,@body)
            (free-translated-object ,var ,type ',(name type) ,param))))))

;;;# Dereferencing Foreign Pointers

(defun mem-ref (ptr type &optional (offset 0))
  "Return the value of TYPE at OFFSET bytes from PTR. If TYPE is aggregate,
we don't return its 'value' but a pointer to it, which is PTR itself."
  (let ((parsed-type (parse-type type)))
    (if (aggregatep parsed-type)
        (inc-pointer ptr offset)
        (%mem-ref ptr (canonicalize parsed-type) offset))))

(define-compiler-macro mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code MEM-REF when TYPE is constant."
  (if (constantp type)
      (if (aggregatep (parse-type (eval type)))
          `(inc-pointer ,ptr ,offset)
          `(%mem-ref ,ptr ,(canonicalize-foreign-type (eval type)) ,offset))
      form))

(defun mem-set (value ptr type &optional (offset 0))
  "Set the value of TYPE at OFFSET bytes from PTR to VALUE."
  (%mem-set value ptr (canonicalize-foreign-type type) offset))

(define-setf-expander mem-ref (ptr type &optional (offset 0) &environment env)
  "SETF expander for MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on MEM-SET to be able
to open-code (SETF MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; if either TYPE or OFFSET are constant, we avoid rebinding them
    ;; so that the compiler macros on MEM-SET and %MEM-SET work. 
    (with-unique-names (store type-tmp offset-tmp)
      (values
       (append (unless (constantp type)   (list type-tmp))
               (unless (constantp offset) (list offset-tmp))
               dummies)
       (append (unless (constantp type)   (list type))
               (unless (constantp offset) (list offset))
               vals)
       (list store)
       `(progn
          (mem-set ,store ,getter
                   ,@(if (constantp type)   (list type)   (list type-tmp))
                   ,@(if (constantp offset) (list offset) (list offset-tmp)))
          ,store)
       `(mem-ref ,getter
                 ,@(if (constantp type)   (list type)   (list type-tmp))
                 ,@(if (constantp offset) (list offset) (list offset-tmp)))))))

(define-compiler-macro mem-set
    (&whole form value ptr type &optional (offset 0))
  "Compiler macro to open-code (SETF MEM-REF) when type is constant."
  (if (constantp type)
      `(%mem-set ,value ,ptr ,(canonicalize-foreign-type (eval type)) ,offset)
      form))

;;;# Dereferencing Foreign Arrays

(defun mem-aref (ptr type &optional (index 0))
  "Like MEM-REF except for accessing 1d arrays."
  (mem-ref ptr type (* index (foreign-type-size type))))

(define-compiler-macro mem-aref (&whole form ptr type &optional (index 0))
  "Compiler macro to open-code MEM-AREF when TYPE (and eventually INDEX)."
  (if (constantp type)
      (if (constantp index)
          `(mem-ref ,ptr ,type
                    ,(* (eval index) (foreign-type-size (eval type))))
          `(mem-ref ,ptr ,type (* ,index ,(foreign-type-size (eval type)))))
      form))

(define-setf-expander mem-aref (ptr type &optional (index 0) &environment env)
  "SETF expander for MEM-AREF."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; we avoid rebinding type and index, if possible (and if type is not
    ;; constant, we don't bother about the index), so that the compiler macros
    ;; on MEM-SET or %MEM-SET can work.
    (with-unique-names (store type-tmp index-tmp)
      (values
       (append (unless (constantp type)
                 (list type-tmp))
               (unless (and (constantp type) (constantp index))
                 (list index-tmp))
               dummies)
       (append (unless (constantp type)
                 (list type))
               (unless (and (constantp type) (constantp index))
                 (list index))
               vals)
       (list store)
       ;; Here we'll try to calculate the offset from the type and index,
       ;; or if not possible at least get the type size early.
       `(progn
          ,(if (constantp type)
               (if (constantp index)
                   `(mem-set ,store ,getter ,type
                             ,(* (eval index) (foreign-type-size (eval type))))
                   `(mem-set ,store ,getter ,type
                             (* ,index-tmp ,(foreign-type-size (eval type)))))
               `(mem-set ,store ,getter ,type-tmp
                         (* ,index-tmp (foreign-type-size ,type-tmp))))
          ,store)
       `(mem-aref ,getter
                  ,@(if (constantp type)
                        (list type)
                        (list type-tmp))
                  ,@(if (and (constantp type) (constantp index))
                        (list index)
                        (list index-tmp)))))))

;;;# Foreign Structures

;;;## Foreign Structure Slots

(defgeneric foreign-struct-slot-pointer (ptr slot)
  (:documentation
   "Get the address of SLOT relative to PTR."))

(defgeneric foreign-struct-slot-pointer-form (ptr slot)
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

(defmethod foreign-struct-slot-pointer (ptr (slot foreign-struct-slot))
  "Return the address of SLOT relative to PTR."
  (inc-pointer ptr (slot-offset slot)))

(defmethod foreign-struct-slot-pointer-form (ptr (slot foreign-struct-slot))
  "Return a form to get the address of SLOT relative to PTR."
  (let ((offset (slot-offset slot)))
    (if (zerop offset)
        ptr
        `(inc-pointer ,ptr ,offset))))

(defun foreign-slot-names (type)
  "Returns a list of TYPE's slot names in no particular order."
  (loop for value being the hash-values
        in (slots (follow-typedefs (parse-type type)))
        collect (slot-name value)))

;;;### Simple Slots

(defclass simple-struct-slot (foreign-struct-slot)
  ()
  (:documentation "Non-aggregate structure slots."))

(defmethod foreign-struct-slot-value (ptr (slot simple-struct-slot))
  "Return the value of a simple SLOT from a struct at PTR."
  (let* ((type (slot-type slot))
         (parsed-type (parse-type type)))
    (translate-from-foreign (mem-ref ptr type (slot-offset slot))
                            parsed-type (name parsed-type))))

(defmethod foreign-struct-slot-value-form (ptr (slot simple-struct-slot))
  "Return a form to get the value of a slot from PTR."
  (let ((type (slot-type slot)))
    (from-c-form type
     `(mem-ref ,ptr ,type ,(slot-offset slot)))))

(defmethod (setf foreign-struct-slot-value) (value ptr (slot simple-struct-slot))
  "Set the value of a simple SLOT to VALUE in PTR."
  (let* ((type (slot-type slot))
         (parsed-type (parse-type type)))
    (setf (mem-ref ptr type (slot-offset slot))
          (translate-to-foreign value parsed-type (name parsed-type)))))

(defmethod foreign-struct-slot-set-form (value ptr (slot simple-struct-slot))
  "Return a form to set the value of a simple structure slot."
  (let ((type (slot-type slot)))
    `(setf (mem-ref ,ptr ,type ,(slot-offset slot))
           ,(to-c-form type value))))

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
  (foreign-struct-slot-pointer ptr slot))

(defmethod foreign-struct-slot-value-form (ptr (slot aggregate-struct-slot))
  "Return a form to get the value of SLOT relative to PTR."
  (foreign-struct-slot-pointer-form ptr slot))

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
  (if (or (> count 1) (aggregatep (parse-type type)))
      (make-instance 'aggregate-struct-slot :offset offset :type type
                     :name name :count count)
      (make-instance 'simple-struct-slot :offset offset :type type
                     :name name)))

;;; Regarding structure alignment, the following ABIs were checked:
;;;   - System-V ABI: x86, x86-64, ppc, arm, mips and itanium. (more?)
;;;   - Mac OS X ABI Function Call Guide: ppc32, ppc64 and x86.
;;;
;;; Rules used here:
;;;
;;;   1. "An entire structure or union object is aligned on the same boundary
;;;       as its most strictly aligned member."
;;;   2. "Each member is assigned to the lowest available offset with the
;;;       appropriate alignment. This may require internal padding, depending
;;;       on the previous member."
;;;   3. "A structure's size is increased, if necessary, to make it a multiple
;;;       of the alignment. This may require tail padding, depending on the last
;;;       member."
;;;
;;; Special case from darwin/ppc32's ABI:
;;; http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/index.html
;;;
;;;   1. "The embedding alignment of the first element in a data structure is
;;;       equal to the element's natural alignment."
;;;   2. "For subsequent elements that have a natural alignment greater than 4
;;;       bytes, the embedding alignment is 4, unless the element is a vector."
;;;       (note: this applies for structures too)

;; FIXME: get a better name for this. --luis
(defun get-alignment (type alignment-type firstp)
  "Return alignment for TYPE according to ALIGNMENT-TYPE."
  (declare (ignorable firstp))
  (ecase alignment-type
    (:normal #-(and darwin ppc32)
             (foreign-type-alignment type)
             #+(and darwin ppc32)
             (if firstp
                 (foreign-type-alignment type)
                 (min 4 (foreign-type-alignment type))))))

(defun adjust-for-alignment (type offset alignment-type firstp)
  "Return OFFSET aligned properly for TYPE according to ALIGNMENT-TYPE."
  (let* ((align (get-alignment type alignment-type firstp))
         (rem (mod offset align)))
    (if (zerop rem)
        offset
        (+ offset (- align rem)))))

(defun notice-foreign-struct-definition (name-and-options slots)
  "Parse and install a foreign structure definition."
  (destructuring-bind (name &key size #+nil alignment)
      (mklist name-and-options)
    (let ((struct (make-instance 'foreign-struct-type :name name))
          (current-offset 0)
          (max-align 1)
          (firstp t))
      ;; determine offsets
      (dolist (slotdef slots)
        (destructuring-bind (slotname type &key (count 1) offset) slotdef
          (setq current-offset
                (or offset
                    (adjust-for-alignment type current-offset :normal firstp)))
          (let* ((slot (make-struct-slot slotname current-offset type count))
                 (align (get-alignment (slot-type slot) :normal firstp)))
            (setf (gethash slotname (slots struct)) slot)
            (when (> align max-align)
              (setq max-align align)))
          (incf current-offset (* count (foreign-type-size type))))
        (setq firstp nil))
      ;; calculate padding and alignment
      (setf (alignment struct) max-align) ; See point 1 above.
      (let ((tail-padding (- max-align (rem current-offset max-align))))
        (unless (= tail-padding max-align) ; See point 3 above.
          (incf current-offset tail-padding)))
      (setf (size struct) (or size current-offset))
      (notice-foreign-type struct))))

(defmacro defcstruct (name &body fields)
  "Define the layout of a foreign structure."
  (discard-docstring fields)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-struct-definition ',name ',fields)))

;;;## Accessing Foreign Structure Slots

(defun get-slot-info (type slot-name)
  "Return the slot info for SLOT-NAME or raise an error."
  (let* ((struct (follow-typedefs (parse-type type)))
         (info (gethash slot-name (slots struct))))
    (unless info
      (error "Undefined slot ~A in foreign type ~A." slot-name type))
    info))

(defun foreign-slot-pointer (ptr type slot-name)
  "Return the address of SLOT-NAME in the structure at PTR."
  (foreign-struct-slot-pointer ptr (get-slot-info type slot-name)))

(defun foreign-slot-offset (type slot-name)
  "Return the offset of SLOT in a struct TYPE."
  (slot-offset (get-slot-info type slot-name)))

;; This is the slow interface to getting the fields of foreign slots.
;; Eventually there will be a compiler macro that optimizes this when
;; the structure type and slot name are known at compile time.
(defun foreign-slot-value (ptr type slot-name)
  "Return the value of SLOT-NAME in the foreign structure at PTR."
  (foreign-struct-slot-value ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-value (&whole form ptr type slot-name)
  "Optimizer for FOREIGN-SLOT-VALUE when TYPE is constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-value-form
       ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(define-setf-expander foreign-slot-value (ptr type slot-name &environment env)
  "SETF expander for FOREIGN-SLOT-VALUE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    (if (and (constantp type) (constantp slot-name))
        ;; if TYPE and SLOT-NAME are constant we avoid rebinding them
        ;; so that the compiler macro on FOREIGN-SLOT-SET works.
        (with-unique-names (store)
          (values
           dummies
           vals
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type ,slot-name)
              ,store)
           `(foreign-slot-value ,getter ,type ,slot-name)))
        ;; if not...
        (with-unique-names (store slot-name-tmp type-tmp)
          (values
           (list* type-tmp slot-name-tmp dummies)
           (list* type slot-name vals)
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type-tmp ,slot-name-tmp)
              ,store)
           `(foreign-slot-value ,getter ,type-tmp ,slot-name-tmp))))))

(defun foreign-slot-set (value ptr type slot-name)
  "Set the value of SLOT-NAME in a foreign structure."
  (setf (foreign-struct-slot-value ptr (get-slot-info type slot-name)) value))

(define-compiler-macro foreign-slot-set
    (&whole form value ptr type slot-name)
  "Optimizer when TYPE and SLOT-NAME are constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-set-form
       value ptr (get-slot-info (eval type) (eval slot-name)))
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

;;; See also the notes regarding ABI requirements in
;;; NOTICE-FOREIGN-STRUCT-DEFINITION
(defun notice-foreign-union-definition (name slots)
  "Parse and install a foreign union definition."
  (let ((struct (make-instance 'foreign-struct-type :name name))
        (max-size 0)
        (max-align 0))
    (dolist (slotdef slots)
      (destructuring-bind (slotname type &key (count 1)) slotdef
        (let* ((slot (make-struct-slot slotname 0 type count))
               (size (* count (foreign-type-size type)))
               (align (foreign-type-alignment (slot-type slot))))
          (setf (gethash slotname (slots struct)) slot)
          (when (> size max-size)
            (setf max-size size))
          (when (> align max-align)
            (setf max-align align)))))
    (setf (size struct) max-size)
    (setf (alignment struct) max-align)
    (notice-foreign-type struct)))

(defmacro defcunion (name &body fields)
  "Define the layout of a foreign union."
  (discard-docstring fields)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-union-definition ',name ',fields)))

;;;# Operations on Types

(defmethod foreign-type-alignment (type)
  "Return the alignment in bytes of a foreign type."
  (foreign-type-alignment (parse-type type)))

(defmethod foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (foreign-type-size (parse-type type)))

(defun foreign-alloc (type &key (initial-element nil initial-element-p)
                      (initial-contents nil initial-contents-p)
                      (count 1 count-p))
  "Allocate enough memory to hold COUNT objects of type TYPE. If
INITIAL-ELEMENT is supplied, each element of the newly allocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the
newly allocated memory."
  (let ((parsed-type (parse-type type))
        (contents-length nil))
    ;; Some error checking, etc...
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (when initial-contents-p
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    ;; Everything looks good.
    (let ((ptr (%foreign-alloc (* (foreign-type-size type) count))))
      (when initial-element-p
        (let ((value (translate-to-foreign
                      initial-element parsed-type (name parsed-type))))
          (dotimes (i count)
            (setf (mem-aref ptr type i) value))))
      (when initial-contents-p
        (dotimes (i contents-length)
          (setf (mem-aref ptr type i)
                (translate-to-foreign
                 (elt initial-contents i) parsed-type (name parsed-type)))))
      ptr)))

;;; Stuff we could optimize here:
;;;   1. (and (constantp type) (constantp count)) => calculate size
;;;   2. (constantp type) => use the translators' expanders
#+nil
(define-compiler-macro foreign-alloc
    (&whole form type &key (initial-element nil initial-element-p)
     (initial-contents nil initial-contents-p) (count 1 count-p))
  )

(defmacro with-foreign-object ((var type &optional (count 1)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated."
  `(with-foreign-pointer
       (,var ,(if (constantp type)
                  (if (constantp count)
                      (* (eval count) (foreign-type-size (eval type)))
                      `(* ,count ,(foreign-type-size (eval type))))
                  `(* ,count (foreign-type-size ,type))))
     ,@body))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;;;# User-defined Types and Translations.

(defmacro define-foreign-type (type lambda-list &body body)
  "Define a parameterized type."
  (discard-docstring body)
  `(progn
     (define-type-spec-parser ,type ,lambda-list
       (make-instance 'foreign-typedef :name ',type
                      :actual-type (parse-type (progn ,@body))))
     ',type))

(defmacro defctype (name base-type &optional docstring)
  "Utility macro for simple C-like typedefs. A similar effect could be
obtained using define-foreign-type."
  (declare (ignore docstring))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-type
      (make-instance 'foreign-typedef :name ',name
                     :actual-type (parse-type ',base-type)))))

;;;## Anonymous Type Translators
;;;
;;; (:wrapper :to-c some-function :from-c another-function)
;;;
;;; TODO: We will need to add a FREE function to this as well I think.
;;; --james

(defclass foreign-type-wrapper (foreign-typedef)
  ((to-c   :initarg :to-c)
   (from-c :initarg :from-c))
  (:documentation "Class for the wrapper type."))

(define-type-spec-parser :wrapper (base-type &key to-c from-c)
  (make-instance 'foreign-type-wrapper 
                 :actual-type (parse-type base-type)
                 :to-c (or to-c 'identity)
                 :from-c (or from-c 'identity)))

(defmethod translate-to-foreign (value (type foreign-type-wrapper) name)
  (let ((actual-type (actual-type type)))
    (translate-to-foreign (funcall (slot-value type 'to-c) value)
                          actual-type (name actual-type))))

(defmethod translate-from-foreign (value (type foreign-type-wrapper) name)
  (let ((actual-type (actual-type type)))
    (funcall (slot-value type 'from-c)
             (translate-from-foreign value actual-type (name actual-type)))))

;;;# Other types

(define-foreign-type :boolean (&optional (base-type :int))
  "Boolean type. Maps to an :int by default. Only accepts integer types."
  (ecase base-type
    ((:char
      :unsigned-char
      :int
      :unsigned-int
      :long
      :unsigned-long) base-type)))

(defmethod translate-to-foreign (value type (name (eql :boolean)))
  (if value 1 0))

(defmethod translate-from-foreign (value type (name (eql :boolean)))
  (not (zerop value)))
