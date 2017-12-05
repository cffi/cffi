;; FIXME: arrange packages so that this can be moved in ASDF some time later?

(in-package #:cffi-toolchain)

(defun static-ops-enabled-p ()
  (ensure-toolchain-parameters)
  (and (or *linkkit-start* *linkkit-end*) t))

(defclass static-runtime-op (monolithic-bundle-op link-op selfward-operation) ()
  (:documentation "Create a Lisp runtime linkable library for the system and its dependencies."))
(defmethod bundle-type ((o static-runtime-op)) :program)
(defmethod selfward-operation ((o static-runtime-op)) 'monolithic-lib-op)

(defmethod output-files ((o static-runtime-op) (s system))
  #-(or ecl mkcl)
  (list (subpathname (component-pathname s)
                     (strcat (coerce-name s) "-runtime")
                     :type (bundle-pathname-type :program))))

(defmethod perform ((o static-runtime-op) (s system))
  (link-lisp-executable
   (output-file o s)
   (link-all-library (first (input-files o s)))))

(defclass static-image-op (image-op) ()
  (:documentation "Create a statically linked standalone image for the system."))
#-(or ecl mkcl) (defmethod selfward-operation ((o static-image-op)) '(load-op static-runtime-op))
#+(or ecl mkcl) (defmethod gather-operation ((o static-image-op)) 'compile-op)
#+(or ecl mkcl) (defmethod gather-operation ((o static-image-op)) :object)

(defclass static-program-op (program-op static-image-op) ()
  (:documentation "Create a statically linked standalone executable for the system."))

;; Problem? Its output may conflict with the program-op output :-/

#-(or ecl mkcl)
(defmethod perform ((o static-image-op) (s system))
  #-(or clisp sbcl) (error "Not implemented yet")
  #+(or clisp sbcl)
  (let* ((name (coerce-name s))
         (runtime (output-file 'static-runtime-op s))
         (image
           #+clisp (implementation-file "base/lispinit.mem")
           #+sbcl (subpathname (lisp-implementation-directory) "sbcl.core"))
         (output (output-file o s))
         (child-op (if (typep o 'program-op) 'program-op 'image-op)))
    (with-temporary-output (tmp output)
      (apply 'invoke runtime
             #+clisp "-M" #+sbcl "--core" image
             `(#+clisp ,@'("--silent" "-ansi" "-norc" "-x")
               #+sbcl ,@'("--noinform" "--non-interactive" "--no-sysinit" "--no-userinit" "--eval")
               ,(with-safe-io-syntax (:package :asdf)
                  (let ((*print-pretty* nil)
                        (*print-case* :downcase))
                    (format
                     ;; This clever staging allows to put things in a single form,
                     ;; as required for CLISP not to print output for the first form,
                     ;; yet allow subsequent forms to rely on packages defined by former forms.
                     nil "'(~@{#.~S~^ ~})"
                     '(require "asdf")
                     '(in-package :asdf)
                     `(progn
                        (setf asdf:*central-registry* ',asdf:*central-registry*)
                        (initialize-source-registry ',asdf::*source-registry-parameter*)
                        (initialize-output-translations ',asdf::*output-translations-parameter*)
                        (upgrade-asdf)
                        ,@(if-let (ql-home
                                   (symbol-value (find-symbol* '*quicklisp-home* 'ql-setup nil)))
                            `((load ,(subpathname ql-home "setup.lisp"))))
                        (load-system "cffi-grovel")
                        ;; We force the (final step of the) operation to take place
                        (defmethod operation-done-p
                            ((operation ,child-op) (system (eql (find-system ,name))))
                          nil)
                        ;; Some implementations (notably SBCL) die as part of dumping an image,
                        ;; so redirect output-files to desired destination, for this processs might
                        ;; never otherwise get a chance to move the file to destination.
                        (defmethod output-files
                            ((operation ,child-op) (system (eql (find-system ,name))))
                          (values (list ,tmp) t))
                        (operate ',child-op ,name)
                        (quit))))))))))

#+(or ecl mkcl)
(defmethod perform ((o static-image-op) (s system))
  (let (#+ecl
        (c::*ld-flags*
         (format nil "-Wl,--export-dynamic ~@[ ~A~]"
                 c::*ld-flags*)))
    (call-next-method)))

;; Allow for :static-FOO-op in ASDF definitions.
(setf (find-class 'asdf::static-runtime-op) (find-class 'static-runtime-op)
      (find-class 'asdf::static-image-op) (find-class 'static-image-op)
      (find-class 'asdf::static-program-op) (find-class 'static-program-op))
