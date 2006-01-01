;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Operations on foreign strings.
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

;;;# Foreign String Conversion
;;;
;;; Functions for converting NULL-terminated C-strings to Lisp strings
;;; and vice versa.  Currently this is blithely ignorant of encoding
;;; and assumes characters can fit in 8 bits.

(defun lisp-string-to-foreign (string ptr size)
  "Copy at most SIZE-1 characters from a Lisp STRING to PTR.
The foreign string will be null-terminated."
  (decf size)
  (loop with i = 0 for char across string
        while (< i size)
        do (%mem-set (char-code char) ptr :unsigned-char (post-incf i))
        finally (%mem-set 0 ptr :unsigned-char i)))

(defun foreign-string-to-lisp (ptr &optional (size most-positive-fixnum)
                               (null-terminated-p t))
  "Copy at most SIZE characters from PTR into a Lisp string.
If PTR is a null pointer, returns nil."
  (unless (null-pointer-p ptr)
    (with-output-to-string (s)
      (loop for i fixnum from 0 below size
            for code = (mem-ref ptr :unsigned-char i)
            until (and null-terminated-p (zerop code))
            do (write-char (code-char code) s)))))

;;;# Using Foreign Strings

(defun foreign-string-alloc (string)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (let* ((length (1+ (length string)))
         (ptr (foreign-alloc :char :count length)))
    (lisp-string-to-foreign string ptr length)
    ptr))

(defun foreign-string-free (ptr)
  "Free a foreign string allocated by FOREIGN-STRING-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-string ((var lisp-string) &body body)
  "Bind VAR to a foreign string containing LISP-STRING in BODY."
  (with-unique-names (str length)
    `(let* ((,str ,lisp-string)
            (,length (progn (check-type ,str string)
                            (1+ (length ,str)))))
       (with-foreign-pointer (,var ,length)
         (lisp-string-to-foreign ,str ,var ,length)
         ,@body))))

(defmacro with-foreign-pointer-as-string
    ((var size &optional size-var) &body body)
  "Like WITH-FOREIGN-PTR except VAR as a Lisp string is used as
the return value of an implcit PROGN around BODY."
  `(with-foreign-pointer (,var ,size ,size-var)
     (progn
       ,@body
       (foreign-string-to-lisp ,var))))

;;;# Automatic Conversion of Foreign Strings

(defctype :string :pointer)

(defmethod translate-to-foreign ((s string) (name (eql :string)))
  (values (foreign-string-alloc s) t))

(defmethod translate-to-foreign (obj (name (eql :string)))
  (if (pointerp obj)
      (values obj nil)
      (error "~A is not a Lisp string or pointer." obj)))

(defmethod translate-from-foreign (ptr (name (eql :string)))
  (foreign-string-to-lisp ptr))

(defmethod free-translated-object (ptr (name (eql :string)) free-p)
  (when free-p
    (foreign-string-free ptr)))

;;; It'd be pretty nice if returning multiple values from translators
;;; worked as expected:
;;;
;;; (define-type-translator :string :from-c (type value)
;;;  "Type translator for string arguments."
;;;  (once-only (value)
;;;    `(values (foreign-string-to-lisp ,value) ,value)))
;;;
;;; For now we'll just define a new type.
;;;
;;; Also as this examples shows, it'd be nice to specify
;;; that we don't want to inherit the from-c translators.
;;; So we could use (defctype :string+ptr :string) and
;;; just add the new :from-c translator.

(defctype :string+ptr :pointer)

(defmethod translate-to-foreign ((s string) (name (eql :string+ptr)))
  (values (foreign-string-alloc s) t))

(defmethod translate-to-foreign (obj (name (eql :string+ptr)))
  (if (pointerp obj)
      (values obj nil)
      (error "~A is not a Lisp string or pointer." obj)))

(defmethod translate-from-foreign (value (name (eql :string+ptr)))
  (list (foreign-string-to-lisp value) value))

(defmethod free-translated-object (value (name (eql :string+ptr)) free-p)
  (when free-p
    (foreign-string-free value)))

