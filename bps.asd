;;;; +----------------------------------------------------------------+
;;;; | Building Problem Solvers                                       |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:register-system-packages "bps"
                               '(#:bps/cps
                                 #:bps/tre
                                 #:bps/ftre
                                 #:bps/jtms
                                 #:bps/ltms
                                 #:bps/tgizmo
                                 #:bps/atms
                                 #:bps/tcon))

(asdf:defsystem #:bps
  :description "Code from Building Problem Solvers"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("bps/cps/all"
               "bps/tre/all"
               "bps/ftre/all"
               "bps/jtms/all"
               "bps/ltms/all"
               "bps/tgizmo/all"
               "bps/atms/all"
               "bps/tcon/all"))
