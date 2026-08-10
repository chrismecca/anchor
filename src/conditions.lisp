;;;; Build conditions.
;;;;
;;;; Build problems are conditions with restarts, not strings collected in a
;;;; list.
;;;;
;;;; A REPL build can stop at the page that caused the problem while the build
;;;; state is still live. That is one reason anchor is written in Lisp.
;;;;
;;;; This file defines only conditions that current code can signal. The rest
;;;; of SPEC.adoc's list should be added with the code that can signal it.
;;;; A condition that nothing signals is only a claim.

(in-package #:anchor)

(define-condition anchor-error (error) ()
  (:documentation "Base class for errors signaled by anchor."))

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
  (:documentation "An _anchor.lisp did not read or did not evaluate."))

(define-condition missing-date (anchor-error)
  ((url :initarg :url :reader missing-date-url))
  (:report (lambda (condition stream)
             (format stream "~a has no :date:, and every other page in its ~
                             section has one."
                     (missing-date-url condition))))
  (:documentation "A page that requires a date does not have one."))

(define-condition usage-error (anchor-error)
  ((message :initarg :message :reader usage-error-message))
  (:report (lambda (condition stream)
             (write-string (usage-error-message condition) stream)))
  (:documentation "The command line was not valid for anchor."))

(define-condition missing-reference (anchor-error)
  ((url :initarg :url :reader missing-reference-url)
   (target :initarg :target :reader missing-reference-target)
   (reference :initarg :reference :reader missing-reference-reference))
  (:report (lambda (condition stream)
             (format stream "~a references ~s, which does not exist."
                     (missing-reference-url condition)
                     (missing-reference-target condition))))
  (:documentation "A link or image target resolved to nothing.

The condition stores the page URL and target directly. This file therefore
does not need definitions that come later, and the report is stable wherever
it is printed."))
