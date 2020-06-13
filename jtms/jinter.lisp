;; -*- Mode: Lisp; -*-

;;;; JTRE definitions
;;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1989 -- 1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(defpackage #:bps/jtms/jinter
  (:use #:cl #:bps/jtms/unify #:bps/jtms/jtms)
  (:export
   #:jtre
   #:jtre-p
   #:jtre-title
   #:jtre-jtms
   #:jtre-dbclass-table
   #:jtre-datum-counter
   #:jtre-rule-counter
   #:jtre-debugging
   #:jtre-queue
   #:jtre-rules-run
   #:*jtre*
   #:with-jtre
   #:in-jtre
   #:debugging-jtre
   #:create-jtre
   #:change-jtre
   #:uassert!
   #:uassume!
   #:run-forms
   #:run
   #:show
   #:run-rules
   #:show-data
   #:show-rules
   #:assert!
   #:assume!
   #:enqueue
   #:view-node))

(in-package #:bps/jtms/jinter)

(defstruct (jtre (:PRINT-FUNCTION jtre-printer))
  title                   ; Pretty name
  jtms                    ; Pointer to its JTMS
  (dbclass-table nil)       ; Table of dbclasses
  (datum-counter 0)       ; Unique ID for asserts
  (rule-counter 0)        ; Unique ID for rules
  (debugging nil)         ; If non-NIL, show basic operations
  (queue nil)             ; Rule queue
  (rules-run 0))          ; Statistic

(defun jtre-printer (j st ignore)
  (declare (ignore ignore))
  (format st "<JTRE: ~A>" (jtre-title j)))

(defvar *JTRE* nil)

(defmacro With-Jtre (jtre &rest forms)
  `(let ((*JTRE* ,jtre)) ,@ forms))

(defun In-Jtre (jtre) (setq *JTRE* jtre))

(defmacro debugging-jtre (msg &rest args)
  `(when (jtre-debugging *JTRE*) (format t ,msg  ,@args)))

(defun create-jtre (title &key debugging)
 (let ((j (make-jtre
           :TITLE title
           :JTMS (create-jtms (list :JTMS-OF title)
                              :NODE-STRING 'view-node)
           :DBCLASS-TABLE (make-hash-table :TEST #'eq)
           :DEBUGGING debugging)))
   (change-jtms (jtre-jtms j)
                :ENQUEUE-PROCEDURE
                #'(lambda (rule) (enqueue rule j)))
   j))

(defun change-jtre (jtre &key (debugging :NADA))
  (unless (eq debugging :NADA)
          (setf (jtre-debugging jtre) debugging)))

;;;; Running JTRE

(defun uassert! (fact &optional (just 'user))
  (assert! fact just) ;; Do internal operation
  (run-rules *JTRE*))        ;; Run the rules

(defun uassume! (fact reason) ;; Similar to UASSERT!
  (assume! fact reason *JTRE*)
  (run-rules *JTRE*))

(defun run-forms (forms &optional (*JTRE* *JTRE*))
  (dolist (form forms)
    (eval form)
    (run-rules *JTRE*)))

(defun run (&optional (*JTRE* *JTRE*)) ;; Toplevel driver function
  (format T "~%>>")
  (let ((*package* (find-package "BPS/JTMS/JINTER")))
    (do ((form (read) (read)))
        ((and (symbolp form)
              (member form '(quit stop exit abort)
                      :test #'string-equal))
         nil)
      (format t "~%~A" (eval form))
      (run-rules)
      (format t "~%>>"))))

(defun show (&optional (*JTRE* *JTRE*) (stream *standard-output*))
  (show-data *JTRE* stream)
  (show-rules *JTRE* stream))
