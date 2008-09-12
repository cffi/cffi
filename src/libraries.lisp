;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; libraries.lisp --- Finding and loading foreign libraries.
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
;;;   (:unix    (:or "libGL.so" "libGL.so.1"
;;;                  #p"/myhome/mylibGL.so"))
;;;   (:windows "opengl32.dll")
;;;   ;; an hypothetical example of a particular platform
;;;   ((:and :some-system :some-cpu) "libGL-support.lib")
;;;   ;; if no other clauses apply, this one will and a type will be
;;;   ;; automagically appended to the name passed to :default
;;;   (t (:default "libGL")))
;;;
;;; This information is stored in the *FOREIGN-LIBRARIES* hashtable
;;; and when the library is loaded through LOAD-FOREIGN-LIBRARY (or
;;; USE-FOREIGN-LIBRARY) the first clause matched by FEATUREP is
;;; processed.

(defvar *foreign-libraries* (make-hash-table :test 'eq)
  "Hashtable of defined libraries.")

(defclass foreign-library ()
  ((spec :initarg :spec)
   (options :initform nil :initarg :options)
   (handle :initarg :handle :accessor foreign-library-handle)))

(defun get-foreign-library (lib)
  "Look up a library by NAME, signalling an error if not found."
  (if (typep lib 'foreign-library)
      lib
      (or (gethash lib *foreign-libraries*)
          (error "Undefined foreign library: ~S" lib))))

(defun (setf get-foreign-library) (value name)
  (setf (gethash name *foreign-libraries*) value))

(defun %foreign-library-spec (lib)
  (assoc-if (lambda (feature)
              (or (eq feature t)
                  (featurep feature)))
            (slot-value lib 'spec)))

(defun foreign-library-spec (lib)
  (second (%foreign-library-spec lib)))

(defun foreign-library-options (lib)
  (append (cddr (%foreign-library-spec lib))
          (slot-value lib 'options)))

;;; Warn about unkown options.
(defmethod initialize-instance :after ((lib foreign-library) &key)
  (loop for (opt nil)
        on (append (slot-value lib 'options)
                   (mapcan (lambda (x) (copy-list (cddr x)))
                           (slot-value lib 'spec)))
        by #'cddr
        when (not (member opt '(:cconv :calling-convention)))
        do (warn "Unkown option: ~A" opt)))

(defmacro define-foreign-library (name-and-options &body pairs)
  "Defines a foreign library NAME that can be posteriorly used with
the USE-FOREIGN-LIBRARY macro."
  (destructuring-bind (name . options)
      (ensure-list name-and-options)
    `(progn
       (setf (get-foreign-library ',name)
             (make-instance 'foreign-library
                            :spec ',pairs :options ',options))
       ',name)))

;;;# LOAD-FOREIGN-LIBRARY-ERROR condition
;;;
;;; The various helper functions that load foreign libraries can
;;; signal this error when something goes wrong. We ignore the host's
;;; error. We should probably reuse its error message.

(define-condition load-foreign-library-error (simple-error)
  ())

(defun read-new-value ()
  (format *query-io* "~&Enter a new value (unevaluated): ")
  (force-output *query-io*)
  (read *query-io*))

(defun fl-error (control &rest arguments)
  (error 'load-foreign-library-error
         :format-control control
         :format-arguments arguments))

;;;# Loading Foreign Libraries

(defun load-darwin-framework (name framework-name)
  "Tries to find and load a darwin framework in one of the directories
in *DARWIN-FRAMEWORK-DIRECTORIES*. If unable to find FRAMEWORK-NAME,
it signals a LOAD-FOREIGN-LIBRARY-ERROR."
  (let ((framework (find-darwin-framework framework-name)))
    (if framework
        (load-foreign-library-path name (native-namestring framework))
        (fl-error "Unable to find framework ~A" framework-name))))

(defun report-simple-error (name error)
  (fl-error "Unable to load foreign library (~A).~%  ~A"
            name
            (format nil "~?" (simple-condition-format-control error)
                    (simple-condition-format-arguments error))))

;;; FIXME: haven't double checked whether all Lisps signal a
;;; SIMPLE-ERROR on %load-foreign-library failure.  In any case they
;;; should be throwing a more specific error.
(defun load-foreign-library-path (name path)
  "Tries to load PATH using %LOAD-FOREIGN-LIBRARY which should try and
find it using the OS's usual methods. If that fails we try to find it
ourselves."
  (handler-case
      (%load-foreign-library name path)
    (error (error)
      (if-let (file (find-file path *foreign-library-directories*))
              (handler-case
                  (%load-foreign-library name (native-namestring file))
                (simple-error (error)
                  (report-simple-error name error)))
              (report-simple-error name error)))))

(defun try-foreign-library-alternatives (name library-list)
  "Goes through a list of alternatives and only signals an error when
none of alternatives were successfully loaded."
  (dolist (lib library-list)
    (when-let (handle (ignore-errors (load-foreign-library-helper name lib)))
      (return-from try-foreign-library-alternatives handle)))
  ;; Perhaps we should show the error messages we got for each
  ;; alternative if we can figure out a nice way to do that.
  (fl-error "Unable to load any of the alternatives:~%   ~S" library-list))

(defparameter *cffi-feature-suffix-map*
  '((:windows . ".dll")
    (:darwin . ".dylib")
    (:unix . ".so")
    (t . ".so"))
  "Mapping of OS feature keywords to shared library suffixes.")

(defun default-library-suffix ()
  "Return a string to use as default library suffix based on the
operating system.  This is used to implement the :DEFAULT option.
This will need to be extended as we test on more OSes."
  (or (cdr (assoc-if #'featurep *cffi-feature-suffix-map*))
      (fl-error "Unable to determine the default library suffix on this OS.")))

(defun load-foreign-library-helper (name thing)
  (etypecase thing
    (string
     (load-foreign-library-path name thing))
    (pathname
     (load-foreign-library-path name (namestring thing)))
    (cons
     (ecase (first thing)
       (:framework (load-darwin-framework name (second thing)))
       (:default
        (unless (stringp (second thing))
          (fl-error "Argument to :DEFAULT must be a string."))
        (load-foreign-library-path
         name (concatenate 'string (second thing) (default-library-suffix))))
       (:or (try-foreign-library-alternatives name (rest thing)))))))

(defun load-foreign-library (library)
  "Loads a foreign LIBRARY which can be a symbol denoting a library defined
through DEFINE-FOREIGN-LIBRARY; a pathname or string in which case we try to
load it directly first then search for it in *FOREIGN-LIBRARY-DIRECTORIES*;
or finally list: either (:or lib1 lib2) or (:framework <framework-name>)."
  (restart-case
      (typecase library
        (symbol
         (let* ((lib (get-foreign-library library))
                (spec (foreign-library-spec lib)))
           (when spec
             (setf (foreign-library-handle lib)
                   (load-foreign-library-helper library spec))
             lib)))
        (t
         (make-instance 'foreign-library :spec (list (list library))
                        :handle (load-foreign-library-helper nil library))))
    ;; Offer these restarts that will retry the call to
    ;; LOAD-FOREIGN-LIBRARY.
    (retry ()
      :report "Try loading the foreign library again."
      (load-foreign-library library))
    (use-value (new-library)
      :report "Use another library instead."
      :interactive read-new-value
      (load-foreign-library new-library))))

(defmacro use-foreign-library (name)
  `(load-foreign-library ',name))

;;;# Closing Foreign Libraries

(defun close-foreign-library (library)
  "Closes a foreign library."
  (let ((lib (get-foreign-library library)))
    (when (foreign-library-handle lib)
      (%close-foreign-library (foreign-library-handle lib))
      (setf (foreign-library-handle lib) nil)
      t)))
