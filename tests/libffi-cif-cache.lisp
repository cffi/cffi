;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; libffi-cif-cache.lisp --- Tests for thread-safe CIF caching.
;;;

(in-package #:cffi-tests)

(deftest libffi-cif-cache.entry-structure
    (let ((entry (cffi::make-libffi-cif-cache-entry)))
      (list (cffi::libffi-cif-cache-entry-p entry)
            (cffi::libffi-cif-cache-entry-cif entry)))
  (t nil))

(deftest libffi-cif-cache.get-or-create
    (let ((entry (cffi::make-libffi-cif-cache-entry)))
      (let* ((first (cffi::get-or-create-cif entry "abs" :int '(:int) :default-abi))
             (second (cffi::get-or-create-cif entry "abs" :int '(:int) :default-abi)))
        (list (pointer-eq first second)
              (cffi:null-pointer-p first))))
  (t nil))

(deftest libffi-cif-cache.repeated-foreign-funcall
    (loop repeat 100 always (= 123 (foreign-funcall "abs" :int -123 :int)))
  t)

#+bordeaux-threads
(deftest libffi-cif-cache.threaded-foreign-funcall
    (let* ((thread-count 8)
           (iterations 250)
           (results (make-array thread-count :initial-element nil))
           (threads (loop for i below thread-count collect
                          (let ((index i))  ; Capture i in a closure
                            (bordeaux-threads:make-thread
                             (lambda ()
                               (setf (aref results index)
                                     (loop repeat iterations
                                           always (= 42 (foreign-funcall "abs" :int -42 :int)))))
                             :name (format nil "libffi-cif-cache-thread-~A" index))))))
      (mapc #'bordeaux-threads:join-thread threads)
      (every #'identity (coerce results 'list)))
  t)

#-bordeaux-threads
(deftest libffi-cif-cache.threaded-foreign-funcall
    t
  t)
