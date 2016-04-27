;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; c-toolchain.lisp --- Generic support compiling and linking C code.
;;;
;;; Copyright (C) 2005-2006, Dan Knap <dankna@accela.net>
;;; Copyright (C) 2005-2006, Emily Backes <lucca@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu <sionescu@cddr.org>
;;; Copyright (C) 2007, Luis Oliveira <loliveira@common-lisp.net>
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

(in-package #:cffi-toolchain)

;;; Utils

(defun parse-command-flags (flags)
  (let ((separators '(#\Space #\Tab #\Newline #\Return)))
    (remove-if 'emptyp (split-string flags :separator separators))))

(defun parse-command-flags-list (strings)
  (loop for flags in strings append (parse-command-flags flags)))

(defun program-argument (x)
  (etypecase x
    (string x)
    (pathname (native-namestring x))))

(defun invoke (command &rest args)
  (when (pathnamep command)
    (setf command (native-namestring command))
    #+os-unix
    (unless (absolute-pathname-p command)
      (setf command (strcat "./" command))))
  (let ((cmd (cons command (mapcar 'program-argument args))))
    (safe-format! *debug-io* "; ~A~%" (escape-command cmd))
    (run-program cmd :output :interactive :error-output :interactive)))


;;; C support

(defparameter *cc*
  #+(or cygwin (not windows)) "cc"
  #+(and windows (not cygwin)) "gcc")

(defparameter *cc-flags*
  (append
   ;; For MacPorts
   #+darwin (list "-I" "/opt/local/include/")
   ;; ECL internal flags
   #+ecl (parse-command-flags c::*cc-flags*)
   ;; FreeBSD non-base header files
   #+freebsd (list "-I" "/usr/local/include/")))


;;; FIXME: is there a better way to detect whether these flags
;;; are necessary?
(defparameter *cpu-word-size-flags*
  #+arm '("-marm")
  #+arm64 '()
  #-(or arm arm64)
  (ecase (cffi:foreign-type-size :pointer)
    (4 '("-m32"))
    (8 '("-m64"))))

(defparameter *shared-library-flags*
  (list #+darwin "-dynamiclib" ;; should we sometimes use -bundle ?
        #-darwin "-shared"))

(defun cc-invocation ()
  `(,(or (getenv "CC") *cc*) ,@*cpu-word-size-flags* ,@*cc-flags*))

(defun call-with-temporary-output (output-file fun)
  (let ((output-file (ensure-pathname output-file :want-file t :ensure-absolute t :truenamize t)))
    (with-temporary-file
        (:pathname tmp :direction :output
         :prefix (strcat (native-namestring (pathname-directory-pathname output-file))
                         (pathname-name output-file) "-tmp")
         :suffix ""
         :type (pathname-type output-file))
      (funcall fun tmp)
      (rename-file-overwriting-target tmp output-file))))

(defmacro with-temporary-output ((output-file-var &optional (output-file-val output-file-var))
                                 &body body)
  "Create an output file atomically, by executing the BODY while OUTPUT-FILE-VAR
is bound to a temporary file name, then atomically renaming that temporary file to OUTPUT-FILE-VAL."
  `(call-with-temporary-output ,output-file-val (lambda (,output-file-var) ,@body)))

(defun invoke-cc (output-file &rest args)
  "Invoke the C Compiler with given OUTPUT-FILE and arguments ARGS"
  (with-temporary-output (output-file)
    (apply 'invoke `(,@(cc-invocation) "-o" ,output-file ,@args))))

(defun cc-compile (output-file inputs)
  (apply 'invoke-cc output-file "-c" #-windows "-fPIC" inputs))

(defun link-executable (output-file inputs)
  (apply 'invoke-cc output-file inputs))

(defun link-static-library (output-file inputs)
  (with-temporary-output (output-file)
    (delete-file-if-exists output-file)
    #+(or bsd linux windows)
    (apply 'invoke
           `(;; TODO: make it portable to BSD.
             ;; ar D is also on FreeBSD, but not on OpenBSD or Darwin, dunno about NetBSD;
             ;; ar T seems to only be on Linux (means something different on Darwin). Sigh.
             ;; A MRI script might be more portable... not, only supported by GNU binutils.
             ;; I couldn't get libtool to work, and it's not ubiquitous anyway.
             ;; "Solution": on BSD, combine objects together using ld -r rather than ar.
             ;; Note that the name .a will then be a lie, since the file is actually a .o;
             ;; but giving the correct name would require a lot of conditionals in other places.
             ;; Sigh. Or we could require libtool on bsd.
             ;; ,@`("libtool" "--mode=link" ,@(cc-invocation) "-static" "-o" ,output-file)
             #+bsd ,@`("ld" "-r" "-o" ,output-file)
             #+linux ,@`("ar" "rcsDT" ,output-file)
             #+windows ,@`("lib" "-nologo" ,(strcat "-out:" (native-namestring output-file)))
             ,@inputs))
    #-(or bsd linux windows)
    (error "Not implemented on your system")))

(defun link-shared-library (output-file inputs)
  ;; remove the library so we won't possibly be overwriting
  ;; the code of any existing process
  (delete-file-if-exists output-file)
  (apply 'invoke-cc output-file
         `(,@*shared-library-flags*
           ,@inputs)))


;;; Computing file names

(defun make-c-file-name (output-defaults &optional suffix)
  (make-pathname :type "c"
                 :name (strcat (pathname-name output-defaults) suffix)
                 :defaults output-defaults))

(defun make-o-file-name (output-defaults &optional suffix)
  (make-pathname :type (asdf/bundle:bundle-pathname-type :object)
                 :name (format nil "~A~@[~A~]" (pathname-name output-defaults) suffix)
                 :defaults output-defaults))

(defun make-lib-file-name (defaults)
  (make-pathname :type (asdf/bundle:bundle-pathname-type :shared-library)
                 :defaults defaults))

(defun make-exe-file-name (defaults)
  (make-pathname :type (asdf/bundle:bundle-pathname-type :program)
                 :defaults defaults))


;;; Implement link-op on image-based platforms.
#-(or clasp ecl mkcl)
(defmethod perform ((o asdf/bundle::link-op) (c system))
  (let* ((inputs (input-files o c))
         (output (first (output-files o c)))
         (kind (asdf/bundle::bundle-type o)))
    (when output ;; some operations skip any output when there is no input
      (ecase kind
        (:program (link-executable output inputs))
        ((:lib :static-library) (link-static-library output inputs))
        ((:dll :shared-library) (link-shared-library output inputs))))))
