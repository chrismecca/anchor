;;;; Serialize an HTML tree.
;;;;
;;;; A layout returns data. The data is a list whose first item is a keyword
;;;; that names an element. Attribute keyword/value pairs follow it. Children
;;;; come after the attributes.
;;;;
;;;;     (:p :class "meta" (:time :datetime "2026-08-05" "August 5, 2026"))
;;;;
;;;; Attributes end at the first item that is not a keyword. That is where
;;;; children begin. A child is never a bare keyword, so no marker is needed.
;;;;
;;;; Layouts return data instead of expanding into code because head-tag
;;;; composition is a list of functions. Each function returns a form. A list
;;;; of forms can be built, filtered, and tested.
;;;;
;;;; A compile-time HTML generator has a different shape. Using one would give
;;;; up that composition. The trade is not worth the small amount of code saved
;;;; here.
;;;;
;;;; Escaping happens here and nowhere else. It happens on strings that have
;;;; never been part of a markup string. (:raw ...) is the documented exception.

(in-package #:anchor)

(defparameter *void-elements*
  '(:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr)
  "Elements written without a closing tag.")

(defparameter *container-elements*
  '(:html :head :body :article :section :nav :header :footer :main :aside
    :div :figure :blockquote :ul :ol)
  "Elements whose children are block-level elements.

A newline follows the opening tag and the closing tag. This keeps nesting
visible in the output.")

(defparameter *block-elements*
  '(:title :meta :link :hr :figcaption :pre :li :p
    :h1 :h2 :h3 :h4 :h5 :h6)
  "Elements that end a line.

Their content is inline. A newline follows the closing tag only. A newline
after the opening tag would put the first word on a separate line.

These rules are cosmetic. They keep anchor's wrapper HTML visually close to
cl-adoc's HTML output.")

(defun escape-text (string stream)
  "Write STRING to STREAM as HTML text content.

Replace `&`, `<`, and `>` with entities."
  (loop for character across string
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (t (write-char character stream)))))

(defun escape-attribute (string stream)
  "Write STRING to STREAM for use in a double-quoted HTML attribute."
  (loop for character across string
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (#\" (write-string "&quot;" stream))
             (t (write-char character stream)))))

(defun element-name (keyword)
  "Return the HTML name for KEYWORD.

For example, :HTTP-EQUIV becomes \"http-equiv\"."
  (string-downcase (symbol-name keyword)))

(defun write-attribute (name value stream)
  "Write one attribute to STREAM.

A NIL value omits the attribute. This lets a layout write optional attributes
without wrapping the whole element in a conditional.

A value of T writes only the attribute name. Use this for boolean attributes."
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
  "Write element FORM to STREAM."
  (let ((tag (first form))
        (rest (rest form)))
    (write-char #\< stream)
    (write-string (element-name tag) stream)
    ;; Write attributes in one pass and leave REST at the first child.
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

When STREAM is supplied, write to it and return NIL. Otherwise return the HTML
as a string.

A NIL form writes nothing. This lets a layout drop an element by returning NIL
where the element would be."
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
        ;; A bare list of forms is what head-tag composition returns.
        (t (dolist (child form) (serialize child stream))))
      (with-output-to-string (out)
        (serialize form out))))
