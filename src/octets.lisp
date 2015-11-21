;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Operations on foreign strings.
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

;;;# Foreign octet vector conversion
;;;
;;; Functions for converting (vector (unsigned-byte 8)) to char*
;;; and vice versa.

(defun octets-to-foreign (octets buffer bufsize &key (start 0) end offset)
  "Copy a lisp octet vector into a foreign buffer."
  (check-type octets (vector (unsigned-byte 8)))
  (when offset
    (incf-pointer buffer offset))
  (unless end
    (setq end (length octets)))
  (loop for i from start below end
       for j below bufsize
       do (setf (mem-aref buffer :unsigned-char j) (aref octets i)))
  buffer)

(defun foreign-to-octets (pointer count)
  "Convert a foreign char* and length to an octet vector."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count)
      (setf (aref octets i) (mem-aref pointer :unsigned-char i)))
    octets))

(defun foreign-octets-alloc (octets &key (start 0) end)
  "Allocate a foreign buffer containing the same bytes as octets.
Returns as values the pointer and the number of bytes allocated."
  (check-type octets (vector (unsigned-byte 8)))
  (unless end
    (setq end (length octets)))
  (let* ((count (- end start))
         (ptr (foreign-alloc :unsigned-char :count count)))
    (octets-to-foreign octets ptr count :start start :end end)
    (values ptr count)))

(defun foreign-octets-free (ptr)
  "Free octets allocated by FOREIGN-OCTETS-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-octets ((ptr size octets &rest args) &body body)
  "PTR and SIZE are not evaluated and are bound to the pointer and size of a
c byte array created with the same content as OCTETS."
  `(multiple-value-bind (,ptr ,size)
       (foreign-octets-alloc ,octets ,@args)
     (unwind-protect
          (progn ,@body)
       (foreign-octets-free ,ptr))))

(defmacro with-foreign-pointer-as-octets ((var-or-vars size) &body body)
  "PTR is bound to a pointer to a foreign buffer of SIZE bytes within BODY.
The return value is constructed by calling FOREIGN-TO-OCTETS on the foreign buffer."
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(with-foreign-pointer (,var ,size ,(or size-var (gensym)))
       ,@body
       (foreign-to-octets ,var ,size-var))))
