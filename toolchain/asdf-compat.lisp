;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; asdf-compat.lisp --- compatibility with older versions of ASDF
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

(in-package #:asdf/bundle)

(defun bundle-pathname-type (bundle-type)
  (etypecase bundle-type
    ((or null string) ;; pass through nil or string literal
     bundle-type)
    ((eql :no-output-file) ;; marker for a bundle-type that has NO output file
     (error "No output file, therefore no pathname type"))
    ((eql :fasl) ;; the type of a fasl
     #-(or clasp ecl mkcl) (compile-file-type) ; on image-based platforms, used as input and output
     #+(or clasp ecl mkcl) "fasb") ; on C-linking platforms, only used as output for system bundles
    ((member :image)
     #+allegro "dxl"
     #+(and clisp os-windows) "exe"
     #-(or allegro (and clisp os-windows)) "image")
    ;; NB: on CLASP and ECL these implementations, we better agree with
    ;; (compile-file-type :type bundle-type))
    ((eql :object) ;; the type of a linkable object file
     (cond ((os-unix-p) "o")
           ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "o" "obj"))))
    ((member :lib :static-library) ;; the type of a linkable library
     (cond ((os-unix-p) "a")
           ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "a" "lib"))))
    ((member :dll :shared-library) ;; the type of a shared library
     (cond ((os-macosx-p) "dylib") ((os-unix-p) "so") ((os-windows-p) "dll")))
    ((eql :program) ;; the type of an executable program
     (cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

(defclass gather-op (bundle-op)
  ((gather-op :initform nil :allocation :class :reader gather-op)
   (gather-type :initform :no-output-file :allocation :class :reader gather-type))
  (:documentation "Abstract operation for gathering many input files from a system"))

(defclass basic-compile-bundle-op (bundle-op)
  ((gather-type :initform #-(or clasp ecl mkcl) :fasl #+(or clasp ecl mkcl) :object
                :allocation :class)
   (bundle-type :initform :fasl :allocation :class)))

(defclass lib-op (link-op gather-op non-propagating-operation)
  ((gather-type :initform :object :allocation :class)
   (bundle-type :initform :lib :allocation :class)))

(defclass compile-bundle-op (basic-compile-bundle-op selfward-operation
                             #+(or clasp ecl mkcl) link-op #-(or clasp ecl) gather-op)
  ((selfward-operation :initform '(prepare-bundle-op #+(or clasp ecl) lib-op)
                       :allocation :class)))

(defclass load-bundle-op (basic-load-op selfward-operation)
  ((selfward-operation :initform '(prepare-bundle-op compile-bundle-op) :allocation :class)))

(defclass dll-op (link-op gather-op non-propagating-operation)
  ((gather-type :initform :object :allocation :class)
   (bundle-type :initform :dll :allocation :class)))

(defclass deliver-asd-op (basic-compile-op selfward-operation)
  ((selfward-operation
    ;; TODO: implement link-op on all implementations, and make that
    ;; '(compile-bundle-op lib-op #-(or clasp ecl mkcl) dll-op)
      :initform '(compile-bundle-op #+(or clasp ecl mkcl) lib-op)
      :allocation :class)))

(defclass monolithic-compile-bundle-op
    (monolithic-bundle-op basic-compile-bundle-op
     #+(or clasp ecl mkcl) link-op gather-op non-propagating-operation)
  ((gather-op :initform #-(or clasp ecl mkcl) 'compile-bundle-op #+(or clasp ecl mkcl) 'lib-op
              :allocation :class)
   (gather-type :initform #-(or clasp ecl mkcl) :fasl #+(or clasp ecl mkcl) :static-library
                :allocation :class)))

(defclass monolithic-lib-op (monolithic-bundle-op lib-op non-propagating-operation)
  ((gather-type :initform :static-library :allocation :class)))


(defclass monolithic-dll-op (monolithic-bundle-op dll-op non-propagating-operation)
  ((gather-type :initform :static-library :allocation :class)))

(defclass image-op (monolithic-bundle-op selfward-operation
                    #+(or clasp ecl mkcl) link-op #+(or clasp ecl mkcl) gather-op)
  ((bundle-type :initform :image)
   #+(or clasp ecl mkcl) (gather-type :initform :static-library :allocation :class)
   (selfward-operation :initform '(#-(or clasp ecl mkcl) load-op) :allocation :class)))

(defun pathname-type-equal-function (type)
  #'(lambda (p) (equal (pathname-type p) type)))

(defmethod input-files ((o gather-op) (c system))
  (unless (eq (bundle-type o) :no-output-file)
    (direct-dependency-files
     o c :key 'output-files
         :test (pathname-type-equal-function (bundle-pathname-type (gather-type o))))))

(defun bundle-output-files (o c)
  (let ((bundle-type (bundle-type o)))
    (unless (or (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
                (and (null (input-files o c)) (not (member bundle-type '(:image :program)))))
      (let ((name (or (component-build-pathname c)
                      (format nil "~A~@[~A~]" (component-name c) (slot-value o 'name-suffix))))
            (type (bundle-pathname-type bundle-type)))
        (values (list (subpathname (component-pathname c) name :type type))
                (eq (type-of o) (coerce-class (component-build-operation c)
                                              :package :asdf/interface
                                              :super 'operation
                                              :error nil)))))))

(defun uiop-library-pathname ()
  #+clasp (probe-file* (compile-file-pathname "sys:uiop" :output-type :object))
  #+ecl (or (probe-file* (compile-file-pathname "sys:uiop" :type :lib)) ;; new style
            (probe-file* (compile-file-pathname "sys:uiop" :type :object))) ;; old style
  #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;uiop"))

(defun asdf-library-pathname ()
  #+clasp (probe-file* (compile-file-pathname "sys:asdf" :output-type :object))
  #+ecl (or (probe-file* (compile-file-pathname "sys:asdf" :type :lib)) ;; new style
            (probe-file* (compile-file-pathname "sys:asdf" :type :object))) ;; old style
  #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;asdf"))

(defun compiler-library-pathname ()
  #+clasp (compile-file-pathname "sys:cmp" :output-type :lib)
  #+ecl (compile-file-pathname "sys:cmp" :type :lib)
  #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:cmp"))

(defmethod component-depends-on :around ((o image-op) (c system))
  (destructuring-bind ((lib-op . deps)) (call-next-method)
    (flet ((has-it-p (x) (find x deps :test 'equal :key 'coerce-name)))
      `((,lib-op
         ,@(unless (or (no-uiop c) (has-it-p "cmp"))
             `(,(make-library-system
                 "cmp" (compiler-library-pathname))))
         ,@(unless (or (no-uiop c) (has-it-p "uiop") (has-it-p "asdf"))
             (cond
               ((system-source-directory :uiop) `(,(find-system :uiop)))
               ((system-source-directory :asdf) `(,(find-system :asdf)))
               (t `(,@(if-let (uiop (uiop-library-pathname))
                              `(,(make-library-system "uiop" uiop)))
                    ,(make-library-system "asdf" (asdf-library-pathname))))))
         ,@deps)))))

(in-package #:asdf/lisp-action)

(defun perform-lisp-compilation (o c)
  (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
        ;; we consult input-files, the first of which should be the one to compile-file
        (input-file (first (input-files o c)))
        ;; on some implementations, there are more than one output-file,
        ;; but the first one should always be the primary fasl that gets loaded.
        (outputs (output-files o c)))
    (multiple-value-bind (output warnings-p failure-p)
        (destructuring-bind
            (output-file
             &optional
               #+(or clasp ecl mkcl) object-file
               #+clisp lib-file
               warnings-file &rest rest) outputs
          ;; Allow for extra outputs that are not of type warnings-file
          ;; The way we do it is kludgy. In ASDF4, output-files shall not be positional.
          (declare (ignore rest))
          (when warnings-file
            (unless (equal (pathname-type warnings-file) (warnings-file-type))
              (setf warnings-file nil)))
          (call-with-around-compile-hook
           c #'(lambda (&rest flags)
                 (apply 'compile-file* input-file
                        :output-file output-file
                        :external-format (component-external-format c)
                        :warnings-file warnings-file
                        (append
                         #+clisp (list :lib-file lib-file)
                         #+(or clasp ecl mkcl) (list :object-file object-file)
                         flags (compile-op-flags o))))))
      (check-lisp-compile-results output warnings-p failure-p
                                  "~/asdf-action::format-action/" (list (cons o c))))))
