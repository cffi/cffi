;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; invoke.lisp --- Half-baked portable run-program.
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

(in-package #:cffi-grovel)

;;;# Shell Execution

#-(or abcl allegro clisp cmu ecl lispworks openmcl sbcl scl)
(error "%INVOKE is unimplemented for this Lisp.  Patches welcome.")

;; FIXME: doesn't do shell quoting
#+abcl
(defun %invoke (command arglist)
  (let ((cmdline (reduce (lambda (str1 str2)
                           (concatenate 'string str1 #\Space str2))
                         arglist :initial-value command))
        (stream (make-string-output-stream)))
    (values (ext:run-shell-command cmdline :output stream)
            (get-output-stream-string stream))))

;;; FIXME: As best I can tell CLISP's EXT:RUN-PROGRAM can either
;;; create new streams OR return the exit code, but not both.  Using
;;; existing streams doesn't seem to be an option either.
#+clisp
(defun %invoke (command arglist)
  (let ((ret (ext:run-program command :arguments arglist)))
    (values (etypecase ret
              ((eql nil) 0)
              ((eql   t) 1)
              (integer   ret))
            "<see above>")))

;;; FIXME: there's no way to tell from EXT:RUN-PROGRAM whether the
;;; command failed or not.  Using EXT:SYSTEM instead, but we should
;;; quote arguments.
#+ecl
(defun %invoke (command arglist)
  (values (ext:system (format nil "~A~{ ~A~}" command arglist))
          "<see above>"))

#+(or openmcl cmu scl sbcl)
(defun %invoke (command arglist)
  (let* ((exit-code)
         (output
          (with-output-to-string (s)
            (let ((process (#+openmcl ccl:run-program
                            #+(or cmu scl) ext:run-program
                            #+sbcl sb-ext:run-program
                            command arglist #-win32 :output #-win32 s
                            :error :output
                            #+sbcl :search #+sbcl t)))
              #+win32
              (write-line "note: SBCL on windows can't redirect output.")
              (setq exit-code
                    #+openmcl (nth-value
                               1 (ccl:external-process-status process))
                    #+sbcl (sb-ext:process-exit-code process)
                    #+(or cmu scl) (ext:process-exit-code process))))))
    (values exit-code output)))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:osi))

#+allegro
(defun %invoke (command arglist)
  (let ((cmd #-mswindows (concatenate 'vector (list command command) arglist)
             #+mswindows (format nil "~A~{ ~A~}" command arglist)))
    (multiple-value-bind (output error-output exit-code)
        (excl.osi:command-output cmd :whole t)
      (declare (ignore error-output))
      (values exit-code output))))

;;; FIXME: Runs shell, and arguments are unquoted.
#+lispworks
(defun %invoke (command arglist)
  (let ((s (make-string-output-stream)))
    (values (sys:call-system-showing-output
             (format nil "~A~{ ~A~}" command arglist)
             :output-stream s :prefix "" :show-cmd nil)
            (get-output-stream-string s))))

;;; Do we really want to suppress the output by default?
(defun invoke (command &rest args)
  (when (pathnamep command)
    (setf command (cffi-sys:native-namestring command)))
  (format *debug-io* "; ~A~{ ~A~}~%" command args)
  (multiple-value-bind (exit-code output)
      (%invoke command args)
    (unless (zerop exit-code)
      (error "External process exited with code ~S.~@
              Command was: ~S~{ ~S~}~@
              Output was:~%~A"
             exit-code command args output))))
