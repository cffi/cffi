;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; c2ffi.lisp --- c2ffi related code
;;;
;;; Copyright (C) 2013, Ryan Pavlik <rpavlik@gmail.com>
;;; Copyright (C) 2015, Attila Lendvai <attila@lendvai.name>
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

(in-package #:cffi/c2ffi)

;;; NOTE: Most of this has been taken over from cl-autowrap.

;;; Note this is rather untested and not very extensive at the moment;
;;; it should probably work on linux/win/osx though. Patches welcome.

(defun local-cpu ()
  #+x86-64 "x86_64"
  #+(and (not (or x86-64 freebsd)) x86) "i686"
  #+(and (not x86-64) x86 freebsd) "i386"
  #+arm "arm"
  #+arm64 "aarch64")

(defun local-vendor ()
  #+(or linux windows) "-pc"
  #+darwin "-apple"
  #+(not (or linux windows darwin)) "-unknown")

(defun local-os ()
  #+(or linux android) "-linux"
  #+windows "-windows-msvc"
  #+darwin "-darwin9"
  #+freebsd "-freebsd"
  #+openbsd "-openbsd")

(defun local-environment ()
  #+linux "-gnu"
  #+(and arm android) "-androideabi"
  #+(and (not arm) android) "-android"
  #+(not (or linux android)) "")

(defun local-arch ()
  (strcat (local-cpu) (local-vendor) (local-os) (local-environment)))

(defparameter *target-archs*
  '("i686-pc-linux-gnu"
    "x86_64-pc-linux-gnu"
    "i686-pc-windows-msvc"
    "x86_64-pc-windows-msvc"
    "i686-apple-darwin9"
    "x86_64-apple-darwin9"
    "i386-unknown-freebsd"
    "x86_64-unknown-freebsd"
    "i386-unknown-openbsd"
    "x86_64-unknown-openbsd"
    "arm-pc-linux-gnu"
    "aarch64-pc-linux-gnu"
    "aarch64-apple-darwin9"
    "arm-unknown-linux-androideabi"
    "aarch64-unknown-linux-android"
    "i686-unknown-linux-android"
    "x86_64-unknown-linux-android"))

(defvar *c2ffi-executable* "c2ffi")
(defvar *c2ffi-extra-arguments* (list))
(defvar *trace-c2ffi* nil)

(defun c2ffi-executable-available? ()
  ;; This is a hack to determine if c2ffi exists; it assumes if it
  ;; doesn't exist, we will get a return code other than 0.
  (zerop (nth-value 2 (uiop:run-program `(,*c2ffi-executable* "-h")
                                        :ignore-error-status t))))

(defun run-program* (program args &key (output (if *trace-c2ffi* *standard-output* nil))
                                    (error-output (if *trace-c2ffi* *error-output* nil))
                                    ignore-error-status)
  (when *trace-c2ffi*
    (format *debug-io* "~&; Invoking: ~A~{ ~A~}~%" program args))
  (zerop (nth-value 2 (uiop:run-program (list* program args) :output output
                                        :error-output error-output
                                        :ignore-error-status ignore-error-status))))

(defun generate-spec-using-c2ffi (input-header-file output-spec-path
                                 &key arch sys-include-paths ignore-error-status)
  "Run c2ffi on `INPUT-HEADER-FILE`, outputting to `OUTPUT-FILE` and
`MACRO-OUTPUT-FILE`, optionally specifying a target triple `ARCH`."
  (format *debug-io* "; cffi/c2ffi is generating ~S~%" output-spec-path)
  (uiop:with-temporary-file (:pathname tmp-macro-file
                             :type "h"
                             :keep *trace-c2ffi*)
    nil ; workaround for an UIOP bug; delme eventually (attila, 2016-01-27).
    :close-stream
    (let* ((arch (when arch (list "--arch" arch)))
           (sys-include-paths (loop
                                :for dir :in sys-include-paths
                                :append (list "--sys-include" dir))))
      ;; Invoke c2ffi to first emit C #define's into TMP-MACRO-FILE. We ask c2ffi
      ;; to first generate a file of C global variables that are assigned the
      ;; value of the corresponding #define's, so that in the second pass below
      ;; the C compiler evaluates for us their right hand side and thus we can
      ;; get hold of their value. This is a kludge and eventually we could/should
      ;; support generating cffi-grovel files, and in grovel mode not rely
      ;; on this kludge anymore.
      (when (run-program* *c2ffi-executable* (append
                                              (list "--driver" "null"
                                                    "--macro-file" (namestring tmp-macro-file))
                                              arch
                                              sys-include-paths
                                              *c2ffi-extra-arguments*
                                              (list (namestring input-header-file)))
                          :output *standard-output*
                          :ignore-error-status ignore-error-status)
        ;; Write a tmp header file that #include's the original input file and
        ;; the above generated macros file which will form the input for our
        ;; final, second pass.
        (uiop:with-temporary-file (:stream tmp-include-file-stream
                                   :pathname tmp-include-file
                                   :type "h"
                                   :keep *trace-c2ffi*)
          (format tmp-include-file-stream "#include \"~A\"~%" input-header-file)
          (format tmp-include-file-stream "#include \"~A\"~%" tmp-macro-file)
          :close-stream
          ;; Invoke c2ffi again to generate the final output.
          (run-program* *c2ffi-executable* (append
                                            (list "--output" (namestring output-spec-path))
                                            arch
                                            sys-include-paths
                                            *c2ffi-extra-arguments*
                                            (list (namestring tmp-include-file)))
                        :output *standard-output*
                        :ignore-error-status ignore-error-status))))))

(defun spec-path (base-name &key version (arch (local-arch)))
  (check-type base-name pathname)
  (make-pathname :defaults base-name
                 :name (strcat (pathname-name base-name)
                               (if version
                                   (strcat "-" version)
                                   "")
                               "."
                               arch)
                 :type "spec"))

(defun find-local-spec (base-name &optional (errorp t))
  (let* ((spec-path (spec-path base-name))
         (probed (probe-file spec-path)))
    (if probed
        spec-path
        (when errorp
          (error "c2ffi spec file not found for base name ~S" base-name)))))

(defun ensure-spec-file-is-up-to-date (header-file-path
                                       &key exclude-archs sys-include-paths version)
  (let ((spec-path (find-local-spec header-file-path nil)))
    (flet ((regenerate-spec-file ()
             (let ((local-arch (local-arch)))
               (unless (c2ffi-executable-available?)
                 (error "No spec found for ~S on arch '~A' and the c2ffi executable was not found"
                        header-file-path local-arch))
               (generate-spec-using-c2ffi header-file-path
                                         (spec-path header-file-path
                                                    :arch local-arch
                                                    :version version)
                                         :arch local-arch
                                         :sys-include-paths sys-include-paths)
               ;; Try to run c2ffi for other architectures, but tolerate failure
               (dolist (arch *target-archs*)
                 (unless (or (string= local-arch arch)
                             (member arch exclude-archs :test #'string=))
                   (unless (generate-spec-using-c2ffi header-file-path
                                                     (spec-path header-file-path
                                                                :arch arch
                                                                :version version)
                                                     :arch arch
                                                     :sys-include-paths sys-include-paths
                                                     :ignore-error-status t)
                     (warn "Failed to generate spec for other arch: ~S" arch))))
               (find-local-spec header-file-path))))
      (if (and spec-path
               (not (zerop (with-input-from-file (s spec-path)
                             (file-length s))))
               (uiop:timestamp< (file-write-date header-file-path)
                                (file-write-date spec-path)))
          spec-path            ; it's up to date, just return it as is
          (restart-case
              (regenerate-spec-file)
            (touch-old-copy ()
              :report (lambda (stream)
                        (format stream "Update the modification time of the out-of-date copy ~S" spec-path))
              ;; Make it only be visible when the spec file exists (but it's out of date)
              :test (lambda (condition)
                      (declare (ignore condition))
                      (not (null spec-path)))
              ;; Update the last modification time. Yes, it's convoluted and wasteful,
              ;; but I can't see any other way.
              (with-staging-pathname (tmp-file spec-path)
                (copy-file spec-path tmp-file))
              ;; The return value of RESTART-CASE
              spec-path))))))
