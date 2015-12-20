;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; errno.lisp --- Access to the OS errno.
;;;
;;; Copyright (C) 2015, Ryan Pavlik <rpavlik@gmail.com>
;;; Copyright (C) 2015, Attila Lendvai <attila@lendvai.name>
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or linux freebsd)
  (progn
    (defcfun (%errno-location "__errno_location") :pointer)
    (defcfun (%strerror "strerror") :string (errno :int)))

  ;; https://msdn.microsoft.com/en-us/library/zc53h9bh.aspx
  ;; TODO what about the character encoding? maybe use _wcserror?
  #+windows
  (progn
    (defcfun (%errno-location "_errno") :pointer)
    (defcfun (%strerror "strerror") :string (errno :int)))

  ;; http://src.gnu-darwin.org/src/sys/sys/errno.h.html
  ;; https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man3/strerror_r.3.html
  #+darwin
  (progn
    (defcfun (%errno-location "__error") :pointer)
    (defcfun (%strerror "strerror") :string (errno :int))))

(defun get-errno-pointer ()
  #+(or linux freebsd windows darwin)
  (%errno-location)

  #-(or linux freebsd windows darwin)
  #.(let ((message "Your Lisp/OS does not support errno access. See src/errno.lisp in CFFI."))
      (cerror "Ignore at compile time" message)
      `(error ,message)))

(define-symbol-macro *errno*
    (mem-ref (get-errno-pointer) :int))

(defun strerror (&optional (errno *errno*))
  (%strerror errno))
