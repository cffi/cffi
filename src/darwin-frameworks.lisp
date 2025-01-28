;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; darwin-frameworks.lisp --- Finding and loading darwin frameworks.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2006-2007, Luis Oliveira  <loliveira@common-lisp.net>
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

(defvar *foreign-library-directories*
  '((explode-path-environment-variable "LD_LIBRARY_PATH")
    (explode-path-environment-variable "DYLD_LIBRARY_PATH")
    (uiop:getcwd)
    (darwin-fallback-library-path))
  "List onto which user-defined library paths can be pushed.")

(defun explode-path-environment-variable (name)
  (mapcar #'uiop:ensure-directory-pathname
          (split-if (lambda (c) (eql #\: c))
                    (uiop:getenv name)
                    :elide)))

(defun darwin-fallback-library-path ()
  (or (explode-path-environment-variable "DYLD_FALLBACK_LIBRARY_PATH")
      (list (merge-pathnames #p"lib/" (user-homedir-pathname))
            #+arm64 #p"/opt/homebrew/lib/"
            #p"/opt/local/lib/"
            #p"/usr/local/lib/"
            #p"/usr/lib/")))

(defun fallback-darwin-framework-directories ()
  (or (explode-path-environment-variable "DYLD_FALLBACK_FRAMEWORK_PATH")
      (list (uiop:getcwd)
            (merge-pathnames #p"Library/Frameworks/" (user-homedir-pathname))
            #p"/Library/Frameworks/"
            #p"/System/Library/Frameworks/")))

(defvar *darwin-framework-directories*
  '((explode-path-environment-variable "DYLD_FRAMEWORK_PATH")
    (fallback-darwin-framework-directories))
  "List of directories where Frameworks are searched for.")

(defun find-darwin-framework (framework-name)
  "Searches for FRAMEWORK-NAME in *DARWIN-FRAMEWORK-DIRECTORIES*."
  (dolist (directory (parse-directories *darwin-framework-directories*))
    (let ((framework-directory
            (merge-pathnames (format nil "~A.framework/" framework-name)
                             directory)))

      (when (probe-file framework-directory)
        (let ((path (merge-pathnames framework-name framework-directory)))
          (return-from find-darwin-framework path))))))

(defun load-darwin-framework (name framework-name)
  "Tries to find and load a darwin framework in one of the directories
in *DARWIN-FRAMEWORK-DIRECTORIES*. If unable to find FRAMEWORK-NAME,
it signals a LOAD-FOREIGN-LIBRARY-ERROR."
  (let ((framework (find-darwin-framework framework-name)))
    (if framework
        (load-foreign-library-path name (native-namestring framework))
        (fl-error "Unable to find framework ~A" framework-name))))
