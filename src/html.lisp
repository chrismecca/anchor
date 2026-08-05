;;;; Serialising an HTML tree.
;;;;
;;;; A layout returns data: a list whose head is a keyword naming an element,
;;;; followed by keyword/value attribute pairs, followed by children. Children
;;;; are strings, numbers, or more of the same.
;;;;
;;;;     (:p :class "meta" (:time :datetime "2026-08-05" "August 5, 2026"))
;;;;
;;;; Attributes run until the first element that is not a keyword, which is
;;;; where the children begin. A child is never a bare keyword, so that
;;;; boundary needs no marker.
;;;;
;;;; Layouts return data rather than expanding into code because head-tag
;;;; composition is a list of functions each returning a form, and a list of
;;;; forms is something you can build, filter and test. That is what anchor
;;;; would give up by depending on a generator that runs at compile time, and
;;;; it is worth more than the hundred lines below.
;;;;
;;;; Escaping happens here and nowhere else, on strings that have never been
;;;; part of a markup string. (:raw ...) is the documented exception.

(in-package #:anchor)

(defparameter *void-elements*
  '(:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr)
  "Elements written without a closing tag.")

(defparameter *container-elements*
  '(:html :head :body :article :section :nav :header :footer :main :aside
    :div :figure :blockquote :ul :ol)
  "Elements whose children are themselves blocks.
A newline follows the opening tag as well as the closing one, so that the
nesting is visible in the output.")

(defparameter *block-elements*
  '(:title :meta :link :hr :figcaption :pre :li :p
    :h1 :h2 :h3 :h4 :h5 :h6)
  "Elements that end a line.
Their content is inline, so a newline follows the closing tag only; breaking
after the opening tag would put the first word on a line of its own.

Both of these are cosmetic. They exist so that the page anchor wraps around
cl-adoc's output does not read as though a different program wrote it.")

(defun escape-text (string stream)
  "Write STRING to STREAM with the three characters that are markup in HTML
text content replaced by entities."
  (loop for character across string
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (t (write-char character stream)))))

(defun escape-attribute (string stream)
  "Write STRING to STREAM escaped for use inside a double-quoted attribute."
  (loop for character across string
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (#\" (write-string "&quot;" stream))
             (t (write-char character stream)))))

(defun element-name (keyword)
  "The HTML name of KEYWORD: :http-equiv is \"http-equiv\"."
  (string-downcase (symbol-name keyword)))

(defun write-attribute (name value stream)
  "Write one attribute to STREAM.
A NIL value omits the attribute entirely, so a layout can write an optional
attribute inline without a conditional around the whole element. A value of T
writes the bare name, for the boolean attributes that carry no value."
  (cond
    ((null value))
    ((eq value t)
     (write-char #\Space stream)
     (write-string (element-name name) stream))
    (t
     (write-char #\Space stream)
     (write-string (element-name name) stream)
     (write-string "=\"" stream)
     (escape-attribute (princ-to-string value) stream)
     (write-char #\" stream))))

(defun serialize-element (form stream)
  "Write the element FORM to STREAM."
  (let ((tag (first form))
        (rest (rest form)))
    (write-char #\< stream)
    (write-string (element-name tag) stream)
    ;; One pass writes the attributes and leaves REST on the first child.
    (loop while (and (consp rest) (keywordp (first rest)))
          do (write-attribute (first rest) (second rest) stream)
             (setf rest (cddr rest)))
    (write-char #\> stream)
    (cond
      ((member tag *void-elements*)
       (when rest
         (error "~a is a void element and cannot have children: ~s" tag form)))
      (t
       (when (member tag *container-elements*)
         (terpri stream))
       (dolist (child rest)
         (serialize child stream))
       (write-string "</" stream)
       (write-string (element-name tag) stream)
       (write-char #\> stream)))
    (when (or (member tag *container-elements*)
              (member tag *block-elements*))
      (terpri stream))))

(defun serialize (form &optional stream)
  "Write FORM to STREAM as HTML.
Writes to STREAM when one is supplied and returns NIL; otherwise returns the
HTML as a string. NIL forms write nothing, so a layout can drop an element by
returning NIL in its place."
  (if stream
      (cond
        ((null form))
        ((stringp form) (escape-text form stream))
        ((numberp form) (escape-text (princ-to-string form) stream))
        ((not (consp form))
         (error "Not an HTML form: ~s" form))
        ((eq (first form) :raw)
         (dolist (chunk (rest form))
           (when chunk (write-string chunk stream))))
        ((keywordp (first form)) (serialize-element form stream))
        ;; A bare list of forms, which is what head-tag composition returns.
        (t (dolist (child form) (serialize child stream))))
      (with-output-to-string (out)
        (serialize form out))))
