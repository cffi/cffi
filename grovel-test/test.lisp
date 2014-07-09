
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

