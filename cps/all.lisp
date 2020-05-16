;;;; +----------------------------------------------------------------+
;;;; | Building Problem Solvers                                       |
;;;; +----------------------------------------------------------------+

(uiop:define-package #:bps/cps/all
  (:nicknames #:bps/cps)
  (:use-reexport
   #:bps/cps/search
   #:bps/cps/variants
   #:bps/cps/match)
  (:use
   #:bps/cps/subways
   #:bps/cps/boston
   #:bps/cps/simplify
   #:bps/cps/algebra))
