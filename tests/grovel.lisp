;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; grovel.lisp --- CFFI-Grovel tests.
;;;
;;; Copyright (C) 2014, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package #:cffi-tests)

(deftest %invoke
    (cffi-grovel::%invoke "echo" '("test"))
  0 "test
")

(defun bug-1395242-helper (enum-type base-type)
  (check-type enum-type (member constantenum cenum))
  (check-type base-type (member :int :unsigned-int))
  (uiop/stream:with-temporary-file (:stream grovel-stream :pathname grovel-file)
    ;; Write the grovel file
    (with-open-stream (*standard-output* grovel-stream)
      (write `(,enum-type (bug-1395242-enum :base-type ,base-type)
                          ((:max-uint32 "UINT_LEAST32_MAX")))))
    ;; Get the value of :inaddr-broadcast
    (let ((lisp-file (cffi-grovel:process-grovel-file grovel-file)))
      (unwind-protect
           (progn
             (load lisp-file)
             (cffi:foreign-enum-value 'bug-1395242-enum :max-uint32))
        (uiop/filesystem:delete-file-if-exists lisp-file)))))

(deftest bug-1395242.constantenum.int
    (bug-1395242-helper 'constantenum :int)
  -1)

(deftest bug-1395242.constantenum.unsigned-int
    (bug-1395242-helper 'constantenum :unsigned-int)
  4294967295)

(deftest bug-1395242.cenum.int
    (bug-1395242-helper 'cenum :int)
  -1)

(deftest bug-1395242.cenum.unsigned-int
    (bug-1395242-helper 'cenum :unsigned-int)
  4294967295)
