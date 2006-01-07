;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; libraries.lisp --- Finding and loading foreign libraries.
;;;
;;; Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2006, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;# Finding Foreign Libraries
;;;
;;; We offer two ways for the user of a CFFI library to define
;;; his/her own library directories: *FOREIGN-LIBRARY-DIRECTORIES*
;;; for regular libraries and *DARWIN-FRAMEWORK-DIRECTORIES* for
;;; Darwin frameworks.
;;;
;;; These two special variables behave similarly to
;;; ASDF:*CENTRAL-REGISTRY* as its arguments are evaluated before
;;; being used. We used our MINI-EVAL instead of the full-blown EVAL
;;; though.
;;;
;;; Only after failing to find a library through the normal ways
;;; (eg: on Linux LD_LIBRARY_PATH, /etc/ld.so.cache, /usr/lib/, /lib)
;;; do we try to find the library ourselves.

(defvar *foreign-library-directories* '()
  "List onto which user-defined library paths can be pushed.")

(defvar *darwin-framework-directories*
  '((merge-pathnames #p"Library/Frameworks/" (user-homedir-pathname))
    #p"/Library/Frameworks/"
    #p"/System/Library/Frameworks/")
  "List of directories where Frameworks are searched for.")

(defun mini-eval (form)
  "Simple EVAL-like function to evaluate the elements of
*FOREIGN-LIBRARY-DIRECTORIES* and *DARWIN-FRAMEWORK-DIRECTORIES*."
  (typecase form
    (cons (apply (car form) (mapcar #'mini-eval (cdr form))))
    (symbol (symbol-value form))
    (t form)))

(defun find-file (path directories)
  "Searches for PATH in a list of DIRECTORIES and returns the first it finds."
  (some (lambda (directory) (probe-file (merge-pathnames path directory)))
        directories))
  
(defun find-darwin-framework (framework-name)
  "Searches for FRAMEWORK-NAME in *DARWIN-FRAMEWORK-DIRECTORIES*."
  (dolist (framework-directory *darwin-framework-directories*)
    (let ((path (make-pathname
                 :name framework-name
                 :directory
                 (append (pathname-directory (mini-eval framework-directory))
                         (list (format nil "~A.framework" framework-name))))))
      (when (probe-file path)
        (return-from find-darwin-framework path)))))

;;;# Defining Foreign Libraries
;;;
;;; Foreign libraries can be defined using the
;;; DEFINE-FOREIGN-LIBRARY macro. Example usage:
;;;
;;; (define-foreign-library opengl
;;;   (:darwin  (:framework "OpenGL"))
;;;   (:unix    (:alternatives "libGL.so" "libGL.so.1"
;;;                            #p"/myhome/mylibGL.so"))
;;;   (:windows "opengl32.dll")
;;;   ;; and a hypothetical example of a particular platform
;;;   ;; where the OpenGL library is split in two.
;;;   ((:and :some-system :some-cpu) "libGL-support.lib" "libGL-main.lib"))
;;;
;;; This information is stored in the *FOREIGN-LIBRARIES* hashtable
;;; and when the library is loaded through LOAD-FOREIGN-LIBRARY (usually
;;; indirectly through the USE-FOREIGN-LIBRARY macro) the first clause
;;; that returns true when passed to CFFI-FEATURE-P is processed.

(defvar *foreign-libraries* (make-hash-table :test 'eq)
  "Hashtable of defined libraries.")

(defun get-foreign-library (name)
  "Look up a library by NAME, signalling an error if not found."
  (or (gethash name *foreign-libraries*)
      (error "Undefined foreign library: ~S" name)))

(defun (setf get-foreign-library) (value name)
  (setf (gethash name *foreign-libraries*) value))

(defmacro define-foreign-library (name &body pairs)
  "Defines a foreign library NAME that can be posteriorly used with
the USE-FOREIGN-LIBRARY macro."
  `(progn
     (setf (get-foreign-library ',name) ',pairs)
     ',name))

(defun cffi-feature-p (feature-expression)
  "Matches a FEATURE-EXPRESSION against the symbols in *FEATURES*
that belong to the CFFI-FEATURES package only."
  (when (eql feature-expression t)
    (return-from cffi-feature-p t))
  (let ((features-package (find-package '#:cffi-features)))
    (flet ((cffi-feature-eq (name feature-symbol)
             (and (eq (symbol-package feature-symbol) features-package)
                  (string= name (symbol-name feature-symbol)))))
      (etypecase feature-expression
        (symbol
         (not (null (member (symbol-name feature-expression) *features*
                            :test #'cffi-feature-eq))))
        (cons
         (ecase (first feature-expression)
           (:and (every #'cffi-feature-p (rest feature-expression)))
           (:or  (some #'cffi-feature-p (rest feature-expression)))
           (:not (not (cffi-feature-p (cadr feature-expression))))))))))

;;;# LOAD-FOREIGN-LIBRARY-ERROR condition
;;;
;;; The various helper functions that load foreign libraries
;;; can signal this error when something goes wrong. We ignore
;;; the host's error. We should probably reuse its error message
;;; but they're usually meaningless.

(define-condition load-foreign-library-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (write-string (text condition) stream))))

(defun read-new-value ()
  (format t "~&Enter a new value (unevaluated): ")
  (force-output)
  (read))

;;; The helper library loading functions will use this function
;;; to signal a LOAD-FOREIGN-LIBRARY-ERROR and offer the user a
;;; couple of restarts.
(defun handle-load-foreign-library-error (argument control &rest arguments)
  (restart-case (error 'load-foreign-library-error
                       :text (format nil "~?" control arguments))
    (retry ()
      :report "Try loading the foreign library again."
      (load-foreign-library argument))
    (use-value (new-library)
      :report "Use another library instead."
      :interactive read-new-value
      (load-foreign-library new-library))))

;;;# Loading Foreign Libraries

(defun load-darwin-framework (framework-name)
  "Tries to find and load a darwin framework in one of the directories
in *DARWIN-FRAMEWORK-DIRECTORIES*. If unable to find FRAMEWORK-NAME,
it signals a LOAD-FOREIGN-LIBRARY-ERROR."
  (let ((framework (find-darwin-framework framework-name)))
    (if framework
        (load-foreign-library framework)
        (handle-load-foreign-library-error
         (cons :framework framework-name)
         "Unable to find framework: ~A" framework-name))))

(defun load-foreign-library-name (name)
  "Tries to load NAME using %LOAD-FOREIGN-LIBRARY which should try and
find it using the OS's usual methods. If that fails we try to find it
ourselves."
  (unless (or (ignore-errors (%load-foreign-library name))
              (let ((file (find-file name *foreign-library-directories*)))
                (when file
                  (%load-foreign-library (namestring file)))))
    (handle-load-foreign-library-error
     name "Unable to load foreign library: ~A" name)))

(defun try-foreign-library-alternatives (library-list)
  "Goes through a list of alternatives and only signals an error when
none of alternatives were successfully loaded."
  (unless (some (lambda (lib)
                  (ignore-errors (load-foreign-library lib)))
                library-list)
    (handle-load-foreign-library-error
     (cons :or library-list)
     "Unable to load any of the alternatives:~%   ~S" library-list)))

(defun load-foreign-library (library)
  "Loads a foreign LIBRARY which can be a symbol denoting a library defined
through DEFINE-FOREIGN-LIBRARY; a pathname or string in which case we try to
load it directly first then search for it in *FOREIGN-LIBRARY-DIRECTORIES*;
or finally list: either (:or lib1 lib2) or (:framework <framework-name>)."
  (etypecase library
    (symbol
     (dolist (library-description (get-foreign-library library) t)
       (when (cffi-feature-p (first library-description))
         (dolist (lib (rest library-description))
           (load-foreign-library lib)))))
    (string
     (load-foreign-library-name library))
    (pathname
     (load-foreign-library-name (namestring library)))
    (cons
     (ecase (first library)
       (:framework (load-darwin-framework (second library)))
       (:or (try-foreign-library-alternatives (rest library)))))))

(defmacro use-foreign-library (name)
  `(eval-when (:load-toplevel :execute #+cmu :compile-toplevel)
     (load-foreign-library ',name)))

;;;# Closing Foreign Libraries
;;;
;;; FIXME: LOAD-FOREIGN-LIBRARY should probably keep track of what
;;; libraries it managed to open and CLOSE-FOREIGN-LIBRARY would then
;;; take a look at that. So, for now, this function is unexported.

(defun close-foreign-library (name)
  "Closes a foreign library NAME."
  (%close-foreign-library (etypecase name
                            (pathname (namestring name))
                            (string name))))