;; ADSF file for fsbv
;; Liam Healy 2009-02-22 09:42:23EST fsbv.asd
;; Time-stamp: <2009-04-11 16:05:30EDT fsbv.asd>
;; $Id: $

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem :fsbv
  :description "Foreign Structures By Value."
  :maintainer "Liam Healy <lhealy@common-lisp.net>"
  :licence "See readme.html"
  :depends-on (:cffi :cffi-grovel :trivial-features)
  ;;:pathname (merge-pathnames "syscalls/" *load-truename*)
  :serial t
  :components
  ((:file "init")
   (cffi-grovel:grovel-file "libffi" :pathname #+unix "libffi-unix")
   (:file "defs" :depends-on (libffi))
   (:file "interface")
   (:file "examples" :depends-on (libffi interface))))
