;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cffi-clamiga.lisp --- CL-Amiga (clamiga) backend for CFFI.
;;;
;;; Builds the CFFI-SYS interface on top of clamiga's FFI package:
;;;   - foreign pointers / typed peek-poke      (ffi:peek-* / ffi:poke-*)
;;;   - dlopen / dlsym                          (ffi:load-library / ffi:symbol-pointer)
;;;   - a libffi-based generic call engine      (ffi:call-foreign)
;;;   - libffi closures for callbacks           (ffi:make-callback)
;;;
;;; These primitives are implemented in C (src/core/builtins_ffi.c) and are
;;; only fully functional on the POSIX host build; on real AmigaOS the
;;; call/closure primitives signal "unsupported" (the Amiga path uses the
;;; library-vector model in the AMIGA.FFI package instead).
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

(in-package #:cffi-sys)

;;;# Mis-features
;;;
;;; clamiga has a single flat symbol namespace (dlsym RTLD_DEFAULT) — there
;;; is no per-library symbol resolution at the cffi-sys level.
(pushnew 'flat-namespace *features*)

;;;# Symbol Case

(defun canonicalize-symbol-name-case (name)
  (declare (string name))
  (string-upcase name))

;;;# Pointers
;;;
;;; A CFFI "foreign pointer" is a clamiga FFI foreign-pointer object.  Its
;;; POINTER-ADDRESS is the real machine address (resolved through clamiga's
;;; side table on the 64-bit host), so MAKE-POINTER / INC-POINTER round-trip
;;; through real addresses.

(deftype foreign-pointer ()
  '(satisfies ffi:foreign-pointer-p))

(defun pointerp (ptr)
  "Return true if PTR is a foreign pointer."
  (ffi:foreign-pointer-p ptr))

(defun pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (ffi:pointer-eq ptr1 ptr2))

(defun null-pointer ()
  "Construct and return a null pointer."
  (ffi:make-foreign-pointer 0))

(defun null-pointer-p (ptr)
  "Return true if PTR is a null pointer."
  (ffi:null-pointer-p ptr))

(defun inc-pointer (ptr offset)
  "Return a pointer pointing OFFSET bytes past PTR."
  (ffi:pointer+ ptr offset))

(defun make-pointer (address)
  "Return a pointer pointing to ADDRESS."
  (ffi:make-foreign-pointer address))

(defun pointer-address (ptr)
  "Return the address pointed to by PTR."
  (ffi:foreign-pointer-address ptr))

;;;# Allocation

(defun %foreign-alloc (size)
  "Allocate SIZE bytes of foreign-addressable memory."
  (ffi:alloc-foreign size))

(defun foreign-free (ptr)
  "Free a pointer PTR allocated by FOREIGN-ALLOC."
  (ffi:free-foreign ptr))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The pointer in VAR
is invalid beyond the dynamic extent of BODY.  If SIZE-VAR is supplied it is
bound to SIZE."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  `(let* ((,size-var ,size)
          (,var (ffi:alloc-foreign ,size-var)))
     (unwind-protect
          (progn ,@body)
       (ffi:free-foreign ,var))))

;;;# Shareable Vectors
;;;
;;; clamiga's GC moves Lisp objects, so we can't hand out a pointer into a
;;; Lisp vector's data.  WITH-POINTER-TO-VECTOR-DATA copies the bytes into
;;; foreign memory and copies any changes back out (a copy-in/copy-out).

(defun make-shareable-byte-vector (size)
  "Create a Lisp vector of SIZE bytes that can be passed to
WITH-POINTER-TO-VECTOR-DATA."
  (make-array size :element-type '(unsigned-byte 8)))

(defun %vector-to-foreign (vector)
  (let* ((len (length vector))
         (ptr (ffi:alloc-foreign (max 1 len))))
    (dotimes (i len) (ffi:poke-u8 ptr (aref vector i) i))
    ptr))

(defun %foreign-to-vector (ptr vector)
  (dotimes (i (length vector))
    (setf (aref vector i) (ffi:peek-u8 ptr i))))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer holding a copy of VECTOR's data; any
changes are copied back into VECTOR after BODY."
  (let ((vec (gensym "VEC")))
    `(let* ((,vec ,vector)
            (,ptr-var (%vector-to-foreign ,vec)))
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (%foreign-to-vector ,ptr-var ,vec))
         (ffi:free-foreign ,ptr-var)))))

;;;# Foreign Types
;;;
;;; The host is LP64 (macOS / 64-bit Linux): long, long-long, and pointer
;;; are 8 bytes.  Map each CFFI built-in type to (size accessor-suffix
;;; call-foreign-keyword).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *type-table*
    ;; cffi-type             size  peek/poke suffix   call-foreign primitive
    '((:char                 1     :i8       :int8)
      (:unsigned-char        1     :u8       :uint8)
      (:short                2     :i16      :int16)
      (:unsigned-short       2     :u16      :uint16)
      (:int                  4     :i32      :int32)
      (:unsigned-int         4     :u32      :uint32)
      (:long                 8     :i64      :int64)
      (:unsigned-long        8     :u64      :uint64)
      (:long-long            8     :i64      :int64)
      (:unsigned-long-long   8     :u64      :uint64)
      (:float                4     :single   :float)
      (:double               8     :double   :double)
      (:pointer              8     :pointer  :pointer)
      (:void                 0     nil       :void)
      ;; explicit-width aliases (used by some libraries directly)
      (:int8                 1     :i8       :int8)
      (:uint8                1     :u8       :uint8)
      (:int16                2     :i16      :int16)
      (:uint16               2     :u16      :uint16)
      (:int32                4     :i32      :int32)
      (:uint32               4     :u32      :uint32)
      (:int64                8     :i64      :int64)
      (:uint64               8     :u64      :uint64)))

  (defun %type-entry (type-keyword)
    (or (assoc type-keyword *type-table*)
        (error "~S is not a supported CFFI type on clamiga" type-keyword)))

  (defun %cffi-type->primitive (type-keyword)
    (fourth (%type-entry type-keyword))))

(defun %foreign-type-size (type-keyword)
  "Return the size in bytes of a foreign type."
  (second (%type-entry type-keyword)))

(defun %foreign-type-alignment (type-keyword)
  "Return the alignment in bytes of a foreign type.  On x86-64 / arm64 the
natural alignment equals the size for every scalar type."
  (second (%type-entry type-keyword)))

;;;# Dereferencing
;;;
;;; %MEM-REF / %MEM-SET dispatch on the (typically constant) type keyword to
;;; the matching ffi:peek-* / ffi:poke-* accessor.

(macrolet
    ((define-mem-accessors ()
       (let ((pairs (remove :void *type-table* :key #'first)))
         `(progn
            (defun %mem-ref (ptr type &optional (offset 0))
              (ecase type
                ,@(loop for (kw nil suffix) in pairs
                        collect `(,kw (,(intern (format nil "PEEK-~A" suffix) :ffi)
                                       ptr offset)))))
            (defun %mem-set (value ptr type &optional (offset 0))
              (ecase type
                ,@(loop for (kw nil suffix) in pairs
                        collect `(,kw (,(intern (format nil "POKE-~A" suffix) :ffi)
                                       ptr value offset))))
              value)
            (define-compiler-macro %mem-ref (&whole form ptr type &optional (offset 0))
              (if (constantp type)
                  (ecase (eval type)
                    ,@(loop for (kw nil suffix) in pairs
                            collect `(,kw (list ',(intern (format nil "PEEK-~A" suffix) :ffi)
                                                ptr offset))))
                  form))
            (define-compiler-macro %mem-set (&whole form value ptr type &optional (offset 0))
              (if (constantp type)
                  (ecase (eval type)
                    ,@(loop for (kw nil suffix) in pairs
                            collect `(,kw (list ',(intern (format nil "POKE-~A" suffix) :ffi)
                                                ptr value offset))))
                  form))))))
  (define-mem-accessors))

;;;# Calling Foreign Functions
;;;
;;; A %FOREIGN-FUNCALL form is parsed into (arg-types arg-values return-type
;;; n-fixed) and expanded into a call to ffi:call-foreign.  The function
;;; address for a named call is resolved once (per call site) and cached in a
;;; load-time cons cell, so repeated calls don't repeat the dlsym.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-funcall-args (args)
    "Return (values prim-arg-types arg-value-forms prim-return-type n-fixed).
A &OPTIONAL marker in ARGS records the fixed/variadic boundary."
    (let ((types '()) (fargs '()) (return-type :void) (n-fixed nil))
      (loop while args do
        (let ((type (pop args)))
          (cond ((eq type '&optional)
                 (setf n-fixed (length types)))
                ((null args)
                 (setf return-type (%cffi-type->primitive type)))
                (t
                 (push (%cffi-type->primitive type) types)
                 (push (pop args) fargs)))))
      ;; Compute N-FIXED before NREVERSE: nreverse mutates TYPES in place, so
      ;; reading (length types) afterwards would see the aliased 1-cons tail.
      (let ((nfixed (or n-fixed (length types))))
        (values (nreverse types) (nreverse fargs) return-type nfixed)))))

(defun %require-symbol (name)
  (or (ffi:symbol-pointer name)
      (error "Foreign symbol ~S not found" name)))

(declaim (inline %cached-symbol))
(defun %cached-symbol (name cell)
  "Resolve NAME on first use, caching the pointer in CELL (a cons) so later
calls skip the lookup.  Deferring to first call means libraries loaded after
this code is loaded are still visible."
  (or (car cell)
      (setf (car cell) (%require-symbol name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %symbol-pointer-form (name)
    (if (constantp name)
        `(%cached-symbol ,name (load-time-value (cons nil nil)))
        `(%require-symbol ,name))))

(defmacro %foreign-funcall (name args &key library convention)
  "Call the named foreign function NAME."
  (declare (ignore library convention))
  (multiple-value-bind (types fargs return-type n-fixed)
      (%parse-funcall-args args)
    `(ffi:call-foreign ,(%symbol-pointer-form name)
                       ,return-type ',types (list ,@fargs) ,n-fixed)))

(defmacro %foreign-funcall-pointer (ptr args &key convention)
  "Call the foreign function at pointer PTR."
  (declare (ignore convention))
  (multiple-value-bind (types fargs return-type n-fixed)
      (%parse-funcall-args args)
    `(ffi:call-foreign ,ptr ,return-type ',types (list ,@fargs) ,n-fixed)))

(defmacro %foreign-funcall-varargs (name fixed varargs &rest args
                                    &key convention library)
  (declare (ignore convention library))
  `(%foreign-funcall ,name ,(append fixed (and varargs (cons '&optional varargs)))
                     ,@args))

(defmacro %foreign-funcall-pointer-varargs (ptr fixed varargs &rest args
                                            &key convention)
  (declare (ignore convention))
  `(%foreign-funcall-pointer ,ptr ,(append fixed (and varargs (cons '&optional varargs)))
                             ,@args))

;;;# Callbacks
;;;
;;; A callback is a libffi closure that re-enters the VM.  We create exactly
;;; one closure per callback NAME and dispatch through a mutable indirection
;;; cell, so redefining a callback just updates the cell — the C-side closure
;;; (and the foreign pointer handed to libraries) stays stable.

(defvar *callbacks* (make-hash-table :test 'eq)
  "Maps a callback name to (list FN-CELL FOREIGN-POINTER).")

(defun %define-callback (name return-type arg-types fn)
  (let ((entry (gethash name *callbacks*)))
    (cond
      (entry
       ;; Redefinition: just swap in the new function.
       (setf (car (first entry)) fn))
      (t
       (let* ((cell (list fn))
              (prim-ret (%cffi-type->primitive return-type))
              (prim-args (mapcar #'%cffi-type->primitive arg-types))
              (ptr (ffi:make-callback
                    prim-ret prim-args
                    (lambda (&rest call-args) (apply (car cell) call-args)))))
         (setf (gethash name *callbacks*) (list cell ptr)))))
    name))

(defmacro %defcallback (name return-type arg-names arg-types body
                        &key convention)
  (declare (ignore convention))
  `(%define-callback ',name ',return-type ',arg-types
                     (lambda ,arg-names ,body)))

(defun %callback (name)
  (let ((entry (gethash name *callbacks*)))
    (unless entry
      (error "Undefined callback: ~S" name))
    (second entry)))

;;;# Loading and Closing Foreign Libraries

(defun %load-foreign-library (name path)
  "Load the shared library at PATH; returns a handle."
  (declare (ignore name))
  (let ((handle (ffi:load-library (native-namestring path))))
    (or handle
        (error "Unable to load foreign library ~A" path))))

(defun %close-foreign-library (handle)
  "Close a foreign library opened by %LOAD-FOREIGN-LIBRARY."
  (ffi:close-library handle))

(defun native-namestring (pathname)
  (namestring pathname))

;;;# Foreign Globals

(defun %foreign-symbol-pointer (name library)
  "Return a pointer to the foreign symbol NAME, or NIL if not found."
  (ffi:symbol-pointer name (when (ffi:foreign-pointer-p library) library)))
