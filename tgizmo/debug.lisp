;; -*- Mode: Lisp; -*-

;;;; Utilities for debugging TGizmo
;;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991, Kenneth D. Forbus, Northwestern University
;;;  and Johan de Kleer, the Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(defpackage #:bps/tgizmo/debug
  (:use #:cl
        #:bps/ltms/all
        #:bps/tgizmo/all)
  (:export))

(in-package #:bps/tgizmo/debug)

(defvar *tgizmo-domain-file* "tnst")

(defvar *ex1* "ex1")
(defvar *ex2* "ex2")
(defvar *ex3* "ex3")

(defvar *default-debugging* '(:PSVS-DDS :IR-DDS))

(defun new (&optional (debugging t)
                      (scenario-file *ex1*)
                      (title "Test Gizmo"))
  (in-tgizmo (create-tgizmo title :DEBUGGING debugging))
  (in-ltre (tgizmo-ltre *tgizmo*))
  ;; (change-ltre *ltre* :debugging t)
  (load "../ltms/setrule")
  (with-ltre *ltre* (load "laws"))
  (with-ltre *ltre* (load *tgizmo-domain-file*))
  (load-scenario scenario-file)
  *tgizmo*)

(defun test-ex1 ()
  (new nil *ex1*)
  (tg-run-rules)
  (find-states))

(defun test-ex1-2 ()
  (new nil *ex1*)
  (assume! '(> (A ((Amount-of water liquid) f)) zero) :EX1-TEST)
  (assume! '(> (A ((Amount-of water liquid) g)) zero) :EX1-TEST)
  (assume! '(Aligned P1) :EX1-TEST)
  (assume! '(> (A (pressure F)) (A (pressure g))) :EX1-TEST))

(defun test-ex2 ()
  (new nil *ex2*) (tg-run-rules)(find-states))

(defun test-ex3 () ;; Check out ambiguous influences
  (new nil *ex3*)
  (assume! '(> (A ((Amount-of water liquid) f)) zero) :EX3-TEST)
  (assume! '(> (A ((Amount-of water liquid) g)) zero) :EX3-TEST)
  (assume! '(> (A ((Amount-of water liquid) h)) zero) :EX3-TEST)
  (assume! '(Aligned P1) :EX3-TEST)
  (assume! '(Aligned P2) :EX3-TEST)
  (assume! '(> (A (pressure F)) (A (pressure G))) :EX3-TEST)
  (assume! '(> (A (pressure G)) (A (pressure H))) :EX3-TEST)
  (tg-run-rules))

(defun test-ex3-2 () ;; Check out ambiguous influences
  (new nil *ex3*)
  (assume! '(> (A ((Amount-of water liquid) f)) zero) :EX3-TEST)
  (assume! '(> (A ((Amount-of water liquid) g)) zero) :EX3-TEST)
  (assume! '(> (A ((Amount-of water liquid) h)) zero) :EX3-TEST)
  (tg-run-rules))

;;;; Measurement Interpretation examples

(defvar *ex2-measurements*
  '((> (D ((amount-of water gas) can)) zero)))

(defvar *ex3-measurements*
  '((> (A ((Amount-of water liquid) f)) zero)
    (> (A ((Amount-of water liquid) g)) zero)
    (> (A ((Amount-of water liquid) h)) zero)
    (< (D ((amount-of water liquid) g)) zero)))

(defvar *ex3-extra-measurements*
  '((> (A ((Amount-of water liquid) f)) zero)
    (> (A ((Amount-of water liquid) g)) zero)
    (> (A ((Amount-of water liquid) h)) zero)
    (< (D ((amount-of water liquid) g)) zero)
    (< (D ((amount-of water liquid) f)) zero)))

(defun tgizmo-shakedown (&aux result)
  (test-ex1)
  (unless (= 8 (setq result (length (tgizmo-states *tgizmo*))))
          (error "Example 1 failed.  Should be 8 states, found ~D." result))
  (format t "~% Passed Example 1.")
  (test-ex2)
  (unless (=  13 (setq result (length (tgizmo-states *tgizmo*))))
          (error "Example 1 failed.  Should be 13 states, found ~D." result))
  (format t "~% Passed Example 2.")
  (mi *ex3* *ex3-measurements*
      :DEBUGGING nil :TITLE "Ex3, basic data")
  (unless (= 9 (setq result (length (tgizmo-states *tgizmo*))))
          (error "Example 3, basic measurements, failed.  Should be 9, was ~D." result))
  (format t "~% Passed Example 3 basic.")
  (mi *ex3* *ex3-extra-measurements*
                         :DEBUGGING nil  :TITLE "Ex3 test -- More data")
  (unless (= 3 (setq result (length (tgizmo-states *tgizmo*))))
          (error "Example 3, extra measurements, failed.  Should be 3, was ~D." result))
  (format t "~% Passed Example 3 extra.")
  (format t "~% TGizmo seems okay.") t)
