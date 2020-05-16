;;;; +----------------------------------------------------------------+
;;;; | Building Problem Solvers                                       |
;;;; +----------------------------------------------------------------+

;;;; System definitions

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:bps
  :description "Code from Building Problem Solvers"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("bps/all"))
