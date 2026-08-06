;;;; Serialising an XML tree.
;;;;
;;;; The feed and the sitemap are XML, and they are built as trees and walked
;;;; for the same reason the pages are. Assembling markup with format strings
;;;; would put escaping back in the hands of whoever remembered to call it,
;;;; which is the failure this whole project came out of.
;;;;
;;;; The form is the same as the HTML serialiser's, and the escaping is the
;;;; same code. What differs is that XML has no void elements: every element
;;;; either closes or closes itself, so there is no list of exceptions to
;;;; keep in step with a standard.

(in-package #:anchor)

(defun xml-declaration ()
  "The declaration every document anchor emits begins with."
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%"))

(defun serialize-xml-element (form stream)
  "Write the element FORM to STREAM as XML."
  (let ((tag (first form))
        (rest (rest form)))
    (write-char #\< stream)
    (write-string (element-name tag) stream)
    (loop while (and (consp rest) (keywordp (first rest)))
          do (write-attribute (first rest) (second rest) stream)
             (setf rest (cddr rest)))
    (setf rest (remove nil rest))
    (cond
      ;; An element with nothing in it closes itself.
      ((null rest)
       (write-string "/>" stream))
      ;; One holding other elements puts each on its own line.
      ((some #'consp rest)
       (write-char #\> stream)
       (terpri stream)
       (dolist (child rest)
         (serialize-xml child stream)
         (terpri stream))
       (format stream "</~a>" (element-name tag)))
      ;; One holding only text stays on one line.
      (t
       (write-char #\> stream)
       (dolist (child rest)
         (serialize-xml child stream))
       (format stream "</~a>" (element-name tag))))))

(defun serialize-xml (form &optional stream)
  "Write FORM to STREAM as XML.
Writes to STREAM when one is supplied and returns NIL; otherwise returns the
XML as a string."
  (if stream
      (cond
        ((null form))
        ((stringp form) (escape-text form stream))
        ((numberp form) (escape-text (princ-to-string form) stream))
        ((not (consp form))
         (error "Not an XML form: ~s" form))
        ((keywordp (first form)) (serialize-xml-element form stream))
        (t (dolist (child form) (serialize-xml child stream))))
      (with-output-to-string (out)
        (serialize-xml form out))))
