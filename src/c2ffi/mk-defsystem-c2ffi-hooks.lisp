(in-package "CFFI/C2FFI")

(defvar *mk-c2ffi-file-compiler-options*
  '(
    :allow-pointer-type-simplification
    :allow-skipping-struct-fields
    :assume-struct-by-value-support
    :prelude
    :output-encoding
    :ffi-type-transformer
    :ffi-name-transformer
    :ffi-name-export-predicate
    :callback-factory
    :foreign-library-name
    :foreign-library-spec
    :emit-generated-name-mappings
    :include-sources
    :exclude-sources
    :include-definitions
    :exclude-definitions))

(defun plist-partition (plist keys)
  ;; Return as values plist-with-keys plist-without-keys
  (let (ret sans)
    (loop (multiple-value-bind (indicator value tail)
	      (get-properties plist keys)
            (unless tail
              (return (values ret (nreconc sans plist))))
	    (setq ret (list* indicator value ret))
            (loop until (eq plist tail) do
		  (push (pop plist) sans)
		  (push (pop plist) sans))
            (setq plist (cddr plist))))))

(defun mk-c2ffi-cc (input-file &rest args &key output-file &allow-other-keys)
  "INPUT-FILE is a C header file.  OUTPUT-FILE is a lisp fasl
file. Generate a spec-file via C2FFI, process the spec-file to a
produce lisp tmp-file with cffi definitions and compile the
tmp-file. Supply processor args via the :compiler-option slot for the
component in the MK:DEFSYSTEM definition. Supply the package name for
the generated definitions via C2FFI-SPEC-PACKAGE."
  (format t "~%~S~%" `(mk-c2ffi-cc ,input-file ,@args))
  (let* ((spec-file
	  (merge-pathnames
	   (make-pathname :directory nil
			  :defaults (cffi/c2ffi::spec-path
				     (pathname input-file)))
	   output-file))
	 package-args
	 processor-args
	 (tmp-file (make-pathname :name
				  (concatenate 'string
					       (pathname-name spec-file)
					       ".cffi-tmp")
                                  :type "lisp"
				  :defaults spec-file)))
    (multiple-value-setq (package-args args)
      (plist-partition args '(:c2ffi-spec-package)))
    (multiple-value-setq (processor-args args)
      (plist-partition args *mk-c2ffi-file-compiler-options*))
    ;; MK-OOS :FORCE wont help!  input-file it is always used. This is
    ;; because when we edit spec-files by hand we don't want to
    ;; regenerate them unless the input (header) file has changed.
    (when (or (not (probe-file spec-file))
	      (> (file-write-date input-file) (file-write-date spec-file)))
      (let ((cffi/c2ffi::*known-archs* nil))
	(cffi/c2ffi::generate-spec-using-c2ffi
	 input-file spec-file
	 ;;:arch "i686-pc-linux-gnu"
	 )))
    (when (or (not (probe-file tmp-file))
	      (> (file-write-date spec-file) (file-write-date tmp-file)))
      (let ((tmp (apply #'cffi/c2ffi::process-c2ffi-spec-file
			spec-file
			(or (second package-args)
			    (error
			     "Specify :c2ffi-spec-package in compiler-options"))
			:output tmp-file
			processor-args)))
	(assert (equalp (namestring tmp) (namestring tmp-file)))))
    (when (or (not (probe-file output-file))
	      (> (file-write-date output-file) (file-write-date tmp-file))))
    (compile-file tmp-file :output-file output-file)))


(defun mk-c2ffi-ld (filespec &rest args)
  (format t "~%~S~%" `(mk-c2ffi-loader ,filespec ,@args))
  (apply #'load filespec args))


(mk:define-language :c2ffi-file
    :compiler 'mk-c2ffi-cc
    :loader 'mk-c2ffi-ld
    :source-extension "h"
    :binary-extension (cdr mk::*filename-extensions*))

(defun mk-clean-c2ffi (system &key dry-run)
  (let (ret)
    (mk::%mk-traverse
     (mk:find-system system)
     #'(lambda (c)
	 (when (and (eq (mk::component-type c) :file)
		    (eq (mk::component-language c) :c2ffi-file))
	   (let* ((input (mk::component-full-pathname c :source))
		  (output (mk::component-full-pathname c :binary))
		  (spec (merge-pathnames
			 (make-pathname :directory nil
					:defaults (cffi/c2ffi::spec-path
						   (pathname input)))
			 output))
		  (tmp (make-pathname :name
				      (concatenate 'string
						   (pathname-name spec)
						   ".cffi-tmp")
                                      :type "lisp"
				      :defaults spec)))
	     (push tmp ret)
	     (push spec ret)
	     (push output ret))))
     nil
     t)
    (prog1
	ret
      (unless dry-run
	(mapcar #'delete-file ret)
	))))
