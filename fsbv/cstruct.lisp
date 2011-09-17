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

(in-package #:cffi-fsbv)

(defun number-of-slots (structure-type)
  "Number of slots in the foreign structure."
  (hash-table-count (cffi::structure-slots structure-type)))

(defmethod libffi-type-pointer :around ((type foreign-struct-type))
  (or (call-next-method)
      (setf (slot-value type 'libffi-type-pointer)
            (let* ((ptr (cffi:foreign-alloc 'ffi-type))
                   (number-of-slots (number-of-slots type))
                   (type-pointer-array
                     (cffi:foreign-alloc :pointer :count (1+ number-of-slots)))
                   (slot-counter 0))
              (with-hash-table-iterator (next-slot (cffi::structure-slots type))
                (multiple-value-bind (resultp slot-name slot)
                    (next-slot)
                  (setf (cffi:mem-aref type-pointer-array :pointer slot-counter)
                        (or (libffi-type-pointer (cffi::slot-type slot))
                            (error "Slot type ~a in foreign structure is unknown to libffi."
                                   (cffi::unparse-type (cffi::slot-type slot)))))
                  (incf slot-counter)))
              (setf
               (cffi:mem-aref type-pointer-array :pointer number-of-slots)
               (cffi:null-pointer)
               ;; The ffi-type
               (cffi:foreign-slot-value ptr 'ffi-type 'size) 0
               (cffi:foreign-slot-value ptr 'ffi-type 'alignment) 0
               (cffi:foreign-slot-value ptr 'ffi-type 'type) +type-struct+
               (cffi:foreign-slot-value ptr 'ffi-type 'elements) type-pointer-array)
              ptr))))

;;; Example
;;; :pa cffi-fsbv
;;; (libffi-type-pointer 'complex)
;;; #.(SB-SYS:INT-SAP #X0063DED0)
;;; (cffi:foreign-slot-value (libffi-type-pointer 'complex) 'ffi-type 'size)
;;; 0
;;; (cffi:foreign-slot-value (libffi-type-pointer 'complex) 'ffi-type 'alignment)
;;; 0
;;; (foreign-slot-value (libffi-type-pointer 'complex) 'ffi-type 'type)
;;; 13
