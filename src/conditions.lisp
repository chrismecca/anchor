;;;; Build conditions.
;;;;
;;;; Build problems are conditions with restarts rather than entries in a list
;;;; of strings, so that a build from a REPL stops at the offending page with
;;;; the state still live. That is the whole reason this is written in Lisp.
;;;;
;;;; Only the conditions the build can currently signal are defined here. The
;;;; rest of SPEC.adoc's list arrives with the code that can raise it; a
;;;; condition nothing signals is a claim the code does not back.

(in-package #:anchor)

(define-condition anchor-error (error) ()
  (:documentation "Base class for every error anchor signals."))

(define-condition unreadable-document (anchor-error)
  ((path :initarg :path :reader unreadable-document-path)
   (cause :initarg :cause :reader unreadable-document-cause))
  (:report (lambda (condition stream)
             (format stream "Could not parse ~a: ~a"
                     (unreadable-document-path condition)
                     (unreadable-document-cause condition))))
  (:documentation "cl-adoc could not parse a document."))
