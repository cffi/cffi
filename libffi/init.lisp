;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; init.lisp --- Load libffi and define basics
;;;
;;; Copyright (C) 2009, 2011 Liam M. Healy
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

(define-foreign-library (libffi)
  (:darwin (:or "libffi.dylib" "libffi32.dylib" "/usr/lib/libffi.dylib"))
  (:solaris (:or "/usr/lib/amd64/libffi.so" "/usr/lib/libffi.so"))
  (:openbsd "libffi.so")
  (:unix (:or "libffi.so.6" "libffi32.so.6" "libffi.so.5" "libffi32.so.5"))
  (:windows (:or "libffi-6.dll" "libffi-5.dll" "libffi.dll"))
  (t (:default "libffi")))

(load-foreign-library 'libffi)

;; FIXME not threadsafe: https://bugs.launchpad.net/cffi/+bug/1474211
(defvar *libffi-type-descriptor-cache* (make-hash-table))

(defun %libffi-type-desciptor-cache-value (type)
  (check-type type (or foreign-type keyword))
  (gethash type *libffi-type-descriptor-cache*))

(defun (setf %libffi-type-desciptor-cache-value) (value type)
  (check-type type (or foreign-type keyword))
  (setf (gethash type *libffi-type-descriptor-cache*)
        value))

(defgeneric make-libffi-type-descriptor (object)
  (:documentation "Build a libffi struct that describes the type for libffi. This will be used as read-only argument when the actual call happens.")
  (:method (object)
    (%libffi-type-desciptor-cache-value object))
  (:method ((object foreign-built-in-type))
    (%libffi-type-desciptor-cache-value (type-keyword object)))
  (:method ((type foreign-pointer-type))
    ;; simplify all pointer types into a void*
    (%libffi-type-desciptor-cache-value :pointer))
  (:method :around (object)
    (let ((result (call-next-method)))
      (assert result () "~S failed on ~S. That's bad."
              'make-libffi-type-descriptor object)
      result))
  (:method ((type foreign-type-alias))
    ;; Set the type pointer on demand for alias types (e.g. typedef, enum, etc)
    (make-libffi-type-descriptor (actual-type type))))

(flet ((populate-with-built-in (type &optional (libffi-name type))
         (let ((descriptor (foreign-symbol-pointer
                            (format nil "ffi_type_~(~a~)" libffi-name))))
           (assert descriptor)
           (setf (%libffi-type-desciptor-cache-value type) descriptor))))
  (map nil #'populate-with-built-in *built-in-float-types*)
  (map nil #'populate-with-built-in *other-builtin-types*)
  ;; Set the type descriptors for integer built-in types
  (dolist (type *built-in-integer-types*)
    (populate-with-built-in
     type
     (format
      nil
      "~aint~d"
      (if (string-equal type "unsigned" :end1 (min 8 (length (string type))))
          "u" "s")
      (* 8 (foreign-type-size type))))))
