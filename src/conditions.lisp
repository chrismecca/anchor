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

(define-condition unreadable-configuration (anchor-error)
  ((path :initarg :path :reader unreadable-configuration-path)
   (cause :initarg :cause :reader unreadable-configuration-cause))
  (:report (lambda (condition stream)
             (format stream "Could not evaluate ~a: ~a"
                     (unreadable-configuration-path condition)
                     (unreadable-configuration-cause condition))))
  (:documentation "An _anchor.lisp did not read or would not evaluate."))

(define-condition missing-reference (anchor-error)
  ((url :initarg :url :reader missing-reference-url)
   (target :initarg :target :reader missing-reference-target)
   (reference :initarg :reference :reader missing-reference-reference))
  (:report (lambda (condition stream)
             (format stream "~a references ~s, which does not exist."
                     (missing-reference-url condition)
                     (missing-reference-target condition))))
  (:documentation "A link or image target that resolved to nothing.
The page URL and the target are carried on the condition rather than read
back off the reference, so that this file needs nothing defined after it and
so that the report reads the same wherever it is printed."))
