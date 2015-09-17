;; FIXME: arrange packages so that this can be moved in ASDF some time later?

(in-package #:cffi-toolchain)

(defun static-ops-enabled-p ()
  #+(or clisp cmucl sbcl)
  (and (version<= "3.1.6" (asdf-version))
       #+sbcl (probe-file* (subpathname (lisp-implementation-directory) "sbcl.o")) ;; sbcl 1.2.17
       t))

(defclass static-runtime-op (monolithic-bundle-op asdf/bundle::link-op selfward-operation)
  ((selfward-operation :initform 'monolithic-lib-op :allocation :class))
  (:documentation "Create a Lisp runtime linkable library for the system and its dependencies."))

(defmethod output-files ((o static-runtime-op) (s system))
  (list (subpathname (component-pathname s)
                     (format nil "~A-runtime" (coerce-name s))
                     :type (asdf/bundle::bundle-pathname-type :program))))

(defun get-shared-objects (executable)
  (loop for line in (run-program
                     `("ldd" ,(native-namestring executable))
                     :output :lines)
        ;; Let's avoid using cl-ppcre just for this... If your filename has a #\>, you lose.
        for end = (position #\( line :from-end t)
        for start = (when end (position #\> line :from-end t :end end))
        for object = (when start (string-trim " " (subseq line (1+ start) end)))
        unless (emptyp object)
        collect object))

#+clisp
(defun clisp-file (x &optional type)
  (subpathname custom:*lib-directory* x
               :type (asdf/bundle:bundle-pathname-type type)))

(defun implementation-link-flags ()
  #-(or clisp cmucl sbcl) (error "Not implemented yet")
  #+clisp ;; TODO: cleanup clisp support.
  `("-Wl,--export-dynamic" "-Wl,--whole-archive"
    ,(clisp-file "base/lisp" :static-library)
    "-Wl,--no-whole-archive"
    ;; It's not clear which exactly of the below objects are or aren't needed.
    ,(clisp-file "base/libgnu" :static-library)
    ,(clisp-file "base/bogomips" :object)
    ;; This is ugly, but I don't know how to make it work otherwise.
    ,(clisp-file "base/calls" :object) "/usr/lib/libavcall.so.0.0.0" "/usr/lib/libcallback.so.0.0.0"
    ,(clisp-file "base/gettext" :object)
    ,(clisp-file "base/modules" :object)
    ,(clisp-file "base/regexi" :object)
    ;; Or should we be using no-readline.a instead? How to tell?
    ;; By searching for libreadline in the get-shared-objects results?
    ,(clisp-file "base/readline" :object)
    ;; #+linux ,(clisp-file "bindings/glibc/linux.o")
    ,@(get-shared-objects (clisp-file "base/lisp.run")))
  #+cmucl
  (let ((init (subpathname lisp::*cmucl-core-path* "exec-init.o"))
        (lisp-lib (subpathname lisp::*cmucl-core-path* "lisp.a")))
    ;; for backward compatibility, with the 10.5 SDK installed, add "-mmacosx-version-min=10.5"
    #+darwin `(,init "-all_load" ,lisp-lib "-rdynamic")
    #+linux `("-Wl,--whole-archive" ,lisp-lib
              "-Wl,--no-whole-archive" ,init "-rdynamic" "-ldl" "-lm"))
  #+sbcl
  `(,(subpathname (lisp-implementation-directory) "sbcl.o")
    "-Wl,--export-dynamic,--no-whole-archive"
    ;;"-Wl,--dynamic-list" ,exported-symbols-file
    ;; TODO: need to get the exact list to work on *all* platforms,
    ;; by looking at all the Config files in sbcl/src/runtime/ -- Ouch!
    ,@(when (featurep :linux) '("-ldl"))
    ,@(when (featurep :sb-thread) '("-lpthread"))
    ,@(when (featurep :sb-core-compression) '("-lz"))
    "-lm"))

(defmethod perform ((o static-runtime-op) (s system))
  (link-executable
   (output-file o s)
   `(#+linux "-Wl,--whole-archive" #+darwin "-all_load"
     ,@(input-files o s) ,@(implementation-link-flags))))

(defclass static-image-op (image-op)
  ((selfward-operation :initform '(load-op static-runtime-op) :allocation :class))
  (:documentation "Create a statically linked standalone image for the system."))

(defclass static-program-op (static-image-op program-op) ()
  (:documentation "Create a statically linked standalone executable for the system."))

;; Problem? Its output may conflict with the program-op output :-/

(defmethod perform ((o static-image-op) (s system))
  #-(or clisp cmucl sbcl) (error "Not implemented yet")
  #+(or clisp cmucl sbcl)
  (let* ((name (coerce-name s))
         (runtime (output-file 'static-runtime-op s))
         (image
           #+clisp (clisp-file "base/lispinit.mem")
           #+cmucl lisp::*cmucl-core-path*
           #+sbcl (subpathname (lisp-implementation-directory) "sbcl.core"))
         (output (output-file o s))
         (child-op (if (typep o 'program-op) 'program-op 'image-op)))
    (with-temporary-output (tmp output)
      (apply 'invoke runtime
             #+clisp "-M" #+cmucl "-core" #+sbcl "--core" image
             `(#+clisp ,@'("--silent" "-ansi" "-norc" "-x")
               #+cmucl ,@'("-quiet" "-noinit" "-nositeinit" "-batch" "-eval")
               #+sbcl ,@'("--noinform" "--non-interactive" "--no-sysinit" "--no-userinit" "--eval")
               ,(with-safe-io-syntax ()
                  (let ((*print-pretty* nil)
                        (*print-case* :downcase))
                    (format
                     ;; This clever staging allows to put things in a single form,
                     ;; as required for CLISP not to print output for the first form,
                     ;; yet allow subsequent forms to rely on packages defined by former forms.
                     nil "'(#.~S #.~S)"
                     '(require "asdf")
                     `(progn
                        ,@(if-let (ql-home (symbol-value
                                            (find-symbol* '*quicklisp-home* 'ql-setup nil)))
                            `((load ,(subpathname ql-home "setup.lisp"))))
                        (initialize-source-registry
                         ,asdf/source-registry:*source-registry-parameter*)
                        (initialize-output-translations
                         ,asdf/output-translations:*output-translations-parameter*)
                        (load-system "cffi-grovel")
                        (defmethod operation-done-p ((operation ,child-op)
                                                     (system (eql (find-system ,name))))
                          nil)
                        (defmethod output-files ((operation ,child-op)
                                                 (system (eql (find-system ,name))))
                          (values (list ,tmp) t))
                        (operate ',child-op ,name)
                        (quit))))))))))

;; Allow for :static-FOO-op in ASDF definitions.
(setf (find-class 'asdf::static-runtime-op) (find-class 'static-runtime-op)
      (find-class 'asdf::static-image-op) (find-class 'static-image-op)
      (find-class 'asdf::static-program-op) (find-class 'static-program-op))
