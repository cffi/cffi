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
    (cffi-grovel::invoke "echo" "test")
  nil nil 0)

(defun grovel-forms (forms &key (quiet t))
  (uiop:with-temporary-file (:stream grovel-stream :pathname grovel-file)
    (with-standard-io-syntax
      (with-open-stream (*standard-output* grovel-stream)
        (let ((*package* (find-package :keyword)))
          (mapc #'write forms))))
    (let ((lisp-file (let ((*debug-io* (if quiet (make-broadcast-stream) *debug-io*)))
                       (cffi-grovel:process-grovel-file grovel-file))))
      (unwind-protect
           (load lisp-file)
        (uiop:delete-file-if-exists lisp-file)))))

(defun bug-1395242-helper (enum-type base-type constant-name)
  (check-type enum-type (member constantenum cenum))
  (check-type base-type string)
  (check-type constant-name string)
  (let ((enum-name (intern (symbol-name (gensym))))
        (base-type-name (intern (symbol-name (gensym)))))
    (grovel-forms `((ctype ,base-type-name ,base-type)
                    (,enum-type (,enum-name :base-type ,base-type-name)
                                ((:value ,constant-name)))))
    (cffi:foreign-enum-value enum-name :value)))

(deftest bug-1395242
    (labels
        ((process-expression (expression)
           (loop for enum-type in '(constantenum cenum)
                 always (destructuring-bind (base-type &rest evaluations) expression
                          (loop for (name expected-value) in evaluations
                                for actual-value = (bug-1395242-helper enum-type base-type name)
                                always (or (= expected-value actual-value)
                                           (progn
                                             (format *error-output*
                                                     "Test failed for case: ~A, ~A, ~A (expected ~A, actual ~A)~%"
                                                     enum-type base-type name expected-value actual-value)
                                             nil)))))))
      (every #'process-expression
             '(("uint8_t" ("UINT8_MAX" 255) ("INT8_MAX" 127) ("INT8_MIN" 128))
               ("int8_t" ("INT8_MIN" -128) ("INT8_MAX" 127) ("UINT8_MAX" -1))
               ("uint16_t" ("UINT16_MAX" 65535) ("INT8_MIN" 65408))
               ("int16_t" ("INT16_MIN" -32768) ("INT16_MAX" 32767) ("UINT16_MAX" -1))
               ("uint32_t" ("UINT32_MAX" 4294967295) ("INT8_MIN" 4294967168))
               ("int32_t" ("INT32_MIN" -2147483648) ("INT32_MAX" 2147483647)))))
  t)

(defvar *grovelled-features*)

(deftest grovel-feature
    (let ((*grovelled-features* nil))
      (grovel-forms `((in-package :cffi-tests)
                      (include "limits.h")
                      (feature grovel-test-feature "CHAR_BIT")
                      (feature :char-bit "CHAR_BIT"
                               :feature-list *grovelled-features*)
                      (feature :inexistent-grovel-feature
                               "INEXISTENT_CFFI_GROVEL_FEATURE"
                               :feature-list *grovelled-features*)))
      (unwind-protect
           (values (and (member 'grovel-test-feature *features*) t)
                   (and (member :char-bit *grovelled-features*) t)
                   (member :inexistent-grovel-feature *grovelled-features*))
        (alexandria:removef *features* 'grovel-test-feature)))
  t t nil)

(deftest autotype
  (let* ((header "struct autotype_struct {
  signed char slot_int8;
  short slot_int16;
  int slot_int32;
  long long slot_int64;
  unsigned char slot_uint8;
  unsigned short slot_uint16;
  unsigned int slot_uint32;
  unsigned long long slot_uint64;
  float slot_float;
  double slot_double;
};

typedef signed char ctype_int8;
typedef short ctype_int16;
typedef int ctype_int32;
typedef long long ctype_int64;
typedef unsigned char ctype_uint8;
typedef unsigned short ctype_uint16;
typedef unsigned int ctype_uint32;
typedef unsigned long long ctype_uint64;
typedef float ctype_float;
typedef double ctype_double;"))
    (uiop:with-temporary-file (:stream output :pathname header-file)
      (write-string header output)
      :close-stream
      (let ((forms `((in-package :cffi-tests)
                     (include ,header-file)
                     (cstruct test-autotype_struct "struct autotype_struct"
                              (slot_int8 "slot_int8" :type :auto)
                              (slot_int16 "slot_int16" :type :auto)
                              (slot_int32 "slot_int32" :type :auto)
                              (slot_int64 "slot_int64" :type :auto)
                              (slot_uint8 "slot_uint8" :type :auto)
                              (slot_uint16 "slot_uint16" :type :auto)
                              (slot_uint32 "slot_uint32" :type :auto)
                              (slot_uint64 "slot_uint64" :type :auto)
                              (slot_float "slot_float" :type :auto)
                              (slot_double "slot_double" :type :auto))
                     (ctype ctype_int8 "ctype_int8")
                     (ctype ctype_int16 "ctype_int16")
                     (ctype ctype_int32 "ctype_int32")
                     (ctype ctype_int64 "ctype_int64")
                     (ctype ctype_uint8 "ctype_uint8")
                     (ctype ctype_uint16 "ctype_uint16")
                     (ctype ctype_uint32 "ctype_uint32")
                     (ctype ctype_uint64 "ctype_uint64")
                     (ctype ctype_float "ctype_float")
                     (ctype ctype_double "ctype_double"))))
        (grovel-forms forms)
        (flet ((slot-type (slot)
                 (cffi:foreign-slot-type '(:struct test-autotype_struct) slot))
               (canonicalize (type-name)
                 (cffi::canonicalize-foreign-type type-name)))
          (assert (eq :int8 (slot-type 'slot_int8)))
          (assert (eq :int16 (slot-type 'slot_int16)))
          (assert (eq :int32 (slot-type 'slot_int32)))
          (assert (eq :int64 (slot-type 'slot_int64)))
          (assert (eq :uint8 (slot-type 'slot_uint8)))
          (assert (eq :uint16 (slot-type 'slot_uint16)))
          (assert (eq :uint32 (slot-type 'slot_uint32)))
          (assert (eq :uint64 (slot-type 'slot_uint64)))
          (assert (eq :float (slot-type 'slot_float)))
          (assert (eq :double (slot-type 'slot_double)))
          ;;
          (assert (eq :char (canonicalize 'ctype_int8)))
          (assert (eq :short (canonicalize 'ctype_int16)))
          (assert (eq :int (canonicalize 'ctype_int32)))
          (assert (eq :long (canonicalize 'ctype_int64)))
          (assert (eq :unsigned-char (canonicalize 'ctype_uint8)))
          (assert (eq :unsigned-short (canonicalize 'ctype_uint16)))
          (assert (eq :unsigned-int (canonicalize 'ctype_uint32)))
          (assert (eq :unsigned-long (canonicalize 'ctype_uint64)))
          (assert (eq :float (canonicalize 'ctype_float)))
          (assert (eq :double (canonicalize 'ctype_double))))
        t)))
  t)
