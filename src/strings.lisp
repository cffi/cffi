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
  (loop for char across string
        for i fixnum from 0 below (1- size)
        do (setf (%mem-ref ptr :unsigned-char i) (char-code char))
        finally (setf (%mem-ref ptr :unsigned-char (1+ i)) 0)))

(defun foreign-string-to-lisp (ptr &optional (size most-positive-fixnum)
                               (null-terminated-p t))
  "Copy at most SIZE characters from PTR into a Lisp string.
If PTR is a null pointer, returns nil."
  (unless (null-ptr-p ptr)
    (with-output-to-string (s)
      (loop for i fixnum from 0 below size
            for code = (mem-ref ptr :unsigned-char i)
            until (and null-terminated-p (zerop code))
            do (write-char (code-char code) s)))))

;;;# Using Foreign Strings

(defun foreign-string-alloc (string)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (let* ((length (1+ (length string)))
         (ptr (foreign-alloc length)))
    (lisp-string-to-foreign string ptr length)
    ptr))

(defun foreign-string-free (ptr)
  "Free a foreign string allocated by FOREIGN-STRING-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-string ((var lisp-string) &body body)
  "Bind VAR to a foreign string containing LISP-STRING in BODY."
  (let ((str (gensym "STR"))
        (length (gensym "LENGTH")))
    `(let* ((,str ,lisp-string)
            (,length (1+ (length ,str))))
       (with-foreign-ptr (,var ,length)
         (lisp-string-to-foreign ,str ,var ,length)
         ,@body))))

(defmacro with-foreign-ptr-as-string
    ((var size &optional size-var) &body body)
  "Like WITH-FOREIGN-PTR except VAR as a Lisp string is used as
the return value of an implcit PROGN around BODY."
  `(with-foreign-ptr (,var ,size ,size-var)
     (progn
       ,@body
       (foreign-string-to-lisp ,var))))

;;;# Automatic Conversion of Foreign Strings

(define-type-translation string :pointer
  "Translation between C and Lisp strings."
  :to-c-arg (lambda (var value body)
              `(with-foreign-string (,var ,value)
                 ,@body))
  :to-c (lambda (value)
          `(foreign-string-alloc ,value))
  :from-c (lambda (value)
            `(foreign-string-to-lisp ,value)))