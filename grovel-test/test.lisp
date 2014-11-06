
(in-package :cffi-grovel-test)
(def-suite :cffi-grovel)
(in-suite :cffi-grovel)

(test test1
  (finishes
    (let* ((input-file (asdf:system-relative-pathname
                        :cffi-grovel-test
                        "grovel-test/rlimit.lisp"))
           (output-file (asdf:system-relative-pathname
                         :cffi-grovel-test
                         "grovel-test/rlimit.grovel.lisp"))
           (tmp-file (cffi-grovel:process-grovel-file
                       input-file output-file)))
      (unwind-protect
           (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
        (delete-file tmp-file))
      (load output-file)))
  (is (numberp (eval (read-from-string "cffi-grovel-test.rlimit::+efault+"))))
  (is (numberp (eval (read-from-string "cffi-grovel-test.rlimit::+rlim-infinity+"))))
  (is (numberp (eval (read-from-string "cffi-grovel-test.rlimit::size-of-rlimit")))))

(test test2
  (signals grovel-error
    (let* ((input-file (asdf:system-relative-pathname
                        :cffi-grovel-test
                        "grovel-test/rusage-error.lisp"))
           (output-file (asdf:system-relative-pathname
                         :cffi-grovel-test
                         "grovel-test/rusage-error.grovel.lisp"))
           (tmp-file (cffi-grovel:process-grovel-file
                       input-file output-file)))
      (unwind-protect
           (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
        (delete-file tmp-file))
      (load output-file)))

  (finishes
    (handler-bind ((grovel-error
                    (lambda (c)
                      (invoke-restart (find-restart 'cffi-grovel::append-string c)))))
      (let* ((input-file (asdf:system-relative-pathname
                          :cffi-grovel-test
                          "grovel-test/rusage-error.lisp"))
             (output-file (asdf:system-relative-pathname
                           :cffi-grovel-test
                           "grovel-test/rusage-error.grovel.lisp"))
             (tmp-file (cffi-grovel:process-grovel-file
                        input-file output-file)))
        (unwind-protect
             (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
          (delete-file tmp-file))
        (load output-file)))))

(test test3
  ;; I borrowed a code from osicat
  (finishes
    (let* ((input-file (asdf:system-relative-pathname
                        :cffi-grovel-test
                        "grovel-test/basic-unixint.lisp"))
           (output-file (asdf:system-relative-pathname
                         :cffi-grovel-test
                         "grovel-test/basic-unixint.grovel.lisp"))
           (tmp-file (cffi-grovel:process-grovel-file
                       input-file output-file)))
      (unwind-protect
           (alexandria:copy-file tmp-file output-file :if-to-exists :supersede)
        (delete-file tmp-file))
      (load output-file))))

