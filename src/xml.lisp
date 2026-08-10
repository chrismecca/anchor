;;;; Serialize an XML tree.
;;;;
;;;; The feed and sitemap are XML. anchor builds them as trees and walks those
;;;; trees for the same reason it walks page trees.
;;;;
;;;; Building markup with format strings would put escaping back in the hands
;;;; of each caller. That is the failure this project avoids.
;;;;
;;;; The form is the same shape as the HTML serializer form. Escaping uses the
;;;; same code. XML differs because it has no void elements. Each element either
;;;; closes itself or has a closing tag.

(in-package #:anchor)

(defun xml-declaration ()
  "Return the XML declaration used by generated XML documents."
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%"))

(defun serialize-xml-element (form stream)
  "Write XML element FORM to STREAM."
  (let ((tag (first form))
        (rest (rest form)))
    (write-char #\< stream)
    (write-string (element-name tag) stream)
    (loop while (and (consp rest) (keywordp (first rest)))
          do (write-attribute (first rest) (second rest) stream)
             (setf rest (cddr rest)))
    (setf rest (remove nil rest))
    (cond
      ;; An empty element closes itself.
      ((null rest)
       (write-string "/>" stream))
      ;; An element that contains elements puts each one on its own line.
      ((some #'consp rest)
       (write-char #\> stream)
       (terpri stream)
       (dolist (child rest)
         (serialize-xml child stream)
         (terpri stream))
       (format stream "</~a>" (element-name tag)))
      ;; An element that contains only text stays on one line.
      (t
       (write-char #\> stream)
       (dolist (child rest)
         (serialize-xml child stream))
       (format stream "</~a>" (element-name tag))))))

(defun serialize-xml (form &optional stream)
  "Write FORM to STREAM as XML.

When STREAM is supplied, write to it and return NIL. Otherwise return the XML
as a string."
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
