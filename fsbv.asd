;; ADSF file for fsbv
;; Liam Healy 2009-02-22 09:42:23EST fsbv.asd
;; Time-stamp: <2009-02-22 09:48:23EST fsbv.asd>
;; $Id: $

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem :fsbv
  :description "Foreign Structures By Value."
  :maintainer "Liam Healy <lhealy@common-lisp.net>"
  :licence "LLGPL"
  ;; Should probably eventually have  :trivial-features
  :depends-on (:cffi :cffi-grovel)
  ;;:pathname (merge-pathnames "syscalls/" *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   ;; Platform-specific files
   (cffi-grovel:grovel-file "libffi"
     :pathname #+unix "libffi-unix")))
