;; ADSF file for fsbv
;; Liam Healy 2009-02-22 09:42:23EST fsbv.asd
;; Time-stamp: <2010-11-30 12:31:28EST fsbv.asd>
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
  ( ;; The following two files do not depend on libffi or any foreign library; they define conversions to and from foreign objects.
   (:file "pkgdcl")
   (:file "convert" :depends-on ("pkgdcl"))
   ;; The following files define the interface to functions using structs by value.
   (:file "init")
   (cffi-grovel:grovel-file "libffi" :pathname #+unix "libffi-unix")
   (:file "defs" :depends-on ("libffi" "pkgdcl"))
   (:file "foreign-object-components" :depends-on ("pkgdcl"))
   (:file "cbuiltin" :depends-on ("pkgdcl"))
   (:file "cstruct" :depends-on ("pkgdcl"))
   (:file "functions" :depends-on ("cstruct" "pkgdcl"))
   ;; If you have the GSL libraries installed and want to try the
   ;; examples, (push :havegsl *features*) before loading.
   #+havegsl
   (:file "examples" :depends-on ("libffi" "cstruct" "foreign-object-components"))))
