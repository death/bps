;;;; +----------------------------------------------------------------+
;;;; | Building Problem Solvers                                       |
;;;; +----------------------------------------------------------------+

(uiop:define-package #:bps/tre/all
  (:nicknames #:bps/tre)
  (:use-reexport
   #:bps/tre/unify
   #:bps/tre/tinter
   #:bps/tre/data
   #:bps/tre/rules)
  (:use
   #:bps/tre/treex1))
