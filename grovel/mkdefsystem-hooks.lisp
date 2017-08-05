(in-package "CFFI-GROVEL")

;;; XXX: cannot have two output components with the same name and
;;; different binary type

;;; XXX: compiles a shared library for each lisp implementation
;;; instead of using a single library for all implementations

(mk:define-language :cffi-grovel
    :compiler #'(lambda (input-file &rest args &key output-file
			 &allow-other-keys)
		  (format t "~%~S~%" `(cffi-grovel-compiler ,input-file ,@args))
		  (format t "~%~S~%" `(process-grovel-file ,input-file ,output-file))
		  (let* ((tmp-file
			  (cffi-grovel:process-grovel-file input-file
							   output-file))
			 (compile-args (copy-list args)))
		    ;; XXX no need to massage compile-args. remove?
		    (assert (getf args :output-file) nil "defsystem did not pass the output-file parameter for the binary pathname")
		    (format t "~%~S~%" `(compile-file ,tmp-file ,@compile-args))
		    (apply #'compile-file tmp-file compile-args)))
    :loader #'(lambda (filespec &rest args)
		(format t "~%~S~%" `(cffi-grovel-loader ,filespec ,@args))
		(assert (not mk::*load-source-instead-of-binary*) nil "Unsupported")
		(assert (not mk::*load-source-if-no-binary*) nil "Unsupported")
		(apply #'load filespec args))
    :source-extension (car mk::*filename-extensions*)
    :binary-extension (cdr mk::*filename-extensions*))

(mk:define-language :cffi-wrapper
    :compiler #'(lambda (input-file &rest args &key output-file
			 &allow-other-keys)
		  (format t "~%~S~%" `(cffi-wrapper-compiler ,input-file ,@args))
		  (let ((lib-soname
			 ;; can't use file-namestring on filename.lisp
			 ;; since make-so-file-name gets called. that
			 ;; calls make-pathme with :type and
			 ;; :defaults.
			 (pathname-name input-file)))
		    (format t "~%~S~%" `(process-wrapper-file
					 ,input-file
					 :output-defaults ,output-file
					 :lib-soname ,lib-soname))
		    (multiple-value-bind (tmp-file so-file)
			(cffi-grovel:process-wrapper-file
			 input-file :output-defaults output-file
			 :lib-soname lib-soname)
		      (format t "~%=> ~S~%" (list tmp-file so-file))
		      (let ((compile-args (copy-list args)))
			;; XXX no need to massage compile-args. remove?
			(assert (getf args :output-file) nil "defsystem did not pass the output-file parameter for the binary pathname")
			(format t "~%~S~%" `(compile-file ,tmp-file ,@compile-args))
			(apply #'compile-file tmp-file compile-args)))))
    :loader #'(lambda (filespec &rest args)
		(format t "~%~S~%" `(cffi-wrapper-loader ,filespec ,@args))
		(assert (not mk::*load-source-instead-of-binary*) nil "Unsupported")
		(assert (not mk::*load-source-if-no-binary*) nil "Unsupported")
		(apply #'load filespec args))
    :source-extension (car mk::*filename-extensions*)
    :binary-extension (cdr mk::*filename-extensions*))


#||
;; HERE
#+nil
(cffi-grovel:process-grovel-file
 "/nfs/alumni/m/madhu/scratch/extern/cffi_0.19.0/examples/grovel-example.lisp"
 "/nfs/alumni/m/madhu/scratch/extern/cffi_0.19.0/examples/grovel-example.lisp")

#+nil
(cffi-grovel:process-grovel-file
 #1="/home/madhu/cl/extern/cffi/examples/grovel-example.lisp"
 "/home/madhu/cl/extern/cffi/examples/grovel-example.processed-grovel-file")

#+nil
(cffi-grovel:process-grovel-file
 #1="/home/madhu/cl/extern/cffi/examples/grovel-example.lisp"
 #1#)

#+nil
(let ((cffi-toolchain::*cc-flags* (cons "-m64" cffi-toolchain::*cc-flags*))
      (cffi-toolchain::*ld-dll-flags* (cons "-m64" cffi-toolchain::*ld-dll-flags*)))
  (cffi-grovel:process-wrapper-file
   #1="/home/madhu/cl/extern/cffi/examples/wrapper-example.lisp"
   :output-defaults
   "/home/madhu/cl/extern/cffi/examples/wrapper-example.processed-wrapper-file"
   :lib-soname
   "wrapper-example"))

(cffi-grovel:process-wrapper-file
 #1="/home/madhu/cl/extern/cffi/examples/wrapper-example.lisp"
 :output-defaults
 (merge-pathnames (file-namestring  #1#) "/dev/shm/")
 :lib-soname
 "wrapper-example"
)
#P"/dev/shm/wrapper-example.grovel-tmp.lisp"
#P"/dev/shm/wrapper-example.so"

;; #P"/home/madhu/cl/extern/cffi/examples/wrapper-example.grovel-tmp.lisp"
;; #P"/home/madhu/cl/extern/cffi/examples/wrapper-example.so"

||#