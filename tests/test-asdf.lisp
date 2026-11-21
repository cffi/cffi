;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; asdf.lisp --- CFFI-Grovel asdf support tests.
;;;
;;; Copyright (C) 2015, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package #:cffi-tests)

(defun linkkit-missing-files ()
  "Files named by this implementation's linkkit that are not installed.
A binary distribution can record paths from the machine it was built on
without shipping the libraries they point at -- Roswell's prebuilt CLISP
24.9.7 asks for /tmp/clisp/../ffcall/lib/libffcall.a -- and then nothing
can be linked, however well CFFI behaves."
  (loop for entry in (append cffi-toolchain:*linkkit-start*
                             cffi-toolchain:*linkkit-end*)
        ;; NORMALIZE-FLAGS turns the entries it resolves into pathnames
        ;; and leaves everything else, flags included, as strings.
        when (and (stringp entry)
                  (not (find (char entry 0) "-+"))
                  (not (uiop:probe-file* entry)))
          collect entry))

;; See lp#1953686.
#-sbcl
(when (cffi-toolchain::static-ops-enabled-p)
  (alexandria:when-let (missing (linkkit-missing-files))
    (format *error-output*
            "~&;; TEST-STATIC-PROGRAM is expected to fail; the linkkit names files that are not installed:~{~%;;   ~a~}~%"
            missing))
  (deftest (test-static-program :expected-to-fail (linkkit-missing-files))
      (progn
        (asdf:operate :static-program-op :cffi-tests/example)
        (let ((program (asdf:output-file :static-program-op :cffi-tests/example)))
          (uiop:run-program `(,(native-namestring program) "1" "2 3") :output :lines)))
    ("Arguments: 1 \"2 3\"" "hello, world!") nil 0))

(deftest test-asdf-load
    (progn
      (asdf:load-system :cffi-tests/example)
      (uiop:symbol-call :cffi-example :check-groveller))
  nil)
