(in-package #:cffi-example)

(define "a0(x)" "+x+x")
(define "a1(x)" "a0(+x+x)")
(define "a2(x)" "a1(+x+x)")
(define "a3(x)" "a2(+x+x)")
(define "a4(x)" "a3(+x+x)")
(define "a5(x)" "a4(+x+x)")

(constant (+a0+ "a0(1)"))
(constant (+a1+ "a1(1)"))
(constant (+a2+ "a2(1)"))
(constant (+a3+ "a3(1)"))
(constant (+a4+ "a4(1)"))
