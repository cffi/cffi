;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cstruct.lisp --- Hook to defcstruct
;;;
;;; Copyright (C) 2009, 2010, 2011 Liam Healy  <lhealy@common-lisp.net>
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

(defun %make-libffi-type-descriptor/struct (type)
  (labels
      ((slot-multiplicity (slot)
         (if (typep slot 'aggregate-struct-slot)
             (slot-count slot)
             1))
       (number-of-items (structure-type)
         "Total number of items in the foreign structure."
         (loop for val being the hash-value of (structure-slots structure-type)
               sum (slot-multiplicity val))))
    (let* ((ptr (foreign-alloc '(:struct ffi-type)))
           (nitems (number-of-items type))
           (type-pointer-array
            (foreign-alloc :pointer :count (1+ nitems))))
      (loop for slot in (slots-in-order type)
            for ltp = (make-libffi-type-descriptor
                       (parse-type (slot-type slot)))
            with slot-counter = 0
            do (if ltp
                   (loop
                         repeat (slot-multiplicity slot)
                         do (setf
                             (mem-aref
                              type-pointer-array :pointer slot-counter)
                             ltp)
                         (incf slot-counter))
                   (error
                    "Slot type ~a in foreign structure is unknown to libffi."
                    (unparse-type (slot-type slot)))))
      (setf (mem-aref type-pointer-array :pointer nitems)
            (null-pointer))
      (macrolet ((store (slot value)
                   `(setf (foreign-slot-value ptr '(:struct ffi-type) ',slot) ,value)))
        (store size 0)
        (store alignment 0)
        (store type +type-struct+)
        (store elements type-pointer-array))
      ptr)))

(defmethod make-libffi-type-descriptor ((type foreign-struct-type))
  (or (call-next-method)
      (setf (%libffi-type-desciptor-cache-value type)
            (%make-libffi-type-descriptor/struct type))))
