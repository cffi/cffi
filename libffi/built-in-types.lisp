;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; built-in-types.lisp -- Define libffi-type-pointers for built-in types and typedefs
;;;
;;; Copyright (C) 2011 Liam M. Healy  <lhealy@common-lisp.net>
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

(defun set-libffi-type-pointer-for-built-in (type &optional (libffi-name type))
  (set-libffi-type-pointer
   type
   (foreign-symbol-pointer (format nil "ffi_type_~(~a~)" libffi-name))))

;;; Set the type pointers for non-integer built-in types
(dolist (type (append *built-in-float-types* *other-builtin-types*))
  (set-libffi-type-pointer-for-built-in type))

;;; Set the type pointers for integer built-in types
(dolist (type *built-in-integer-types*)
  (set-libffi-type-pointer-for-built-in
   type
   (format
    nil
    "~aint~d"
    (if (string-equal type "unsigned" :end1 (min 8 (length (string type))))
        "u" "s")
    (* 8 (foreign-type-size type)))))

;;; Set the type pointer on demand for alias (e.g. typedef) types
(defmethod libffi-type-pointer ((type foreign-type-alias))
  (libffi-type-pointer (follow-typedefs type)))

;;; Luis thinks this is unnecessary; FOREIGN-ENUM inherits from FOREIGN-TYPE-ALIAS.
#+(or)
(defmethod libffi-type-pointer ((type foreign-enum))
  (libffi-type-pointer (actual-type type)))
