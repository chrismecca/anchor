;;;; HTML serialization tests.

(in-package #:anchor/tests)

(defun html (form)
  (anchor::serialize form))

(define-test serialize)

(define-test (serialize elements)
  (is string= "<span>text</span>" (html '(:span "text")))
  (is string= "<p>a<span>b</span>c</p>
" (html '(:p "a" (:span "b") "c")))
  (is string= "" (html nil))
  (is string= "<span>7</span>" (html '(:span 7))))

(define-test (serialize attributes)
  (is string= "<a href=\"/x/\">go</a>" (html '(:a :href "/x/" "go")))
  (is string= "<a href=\"/x/\" rel=\"me\">go</a>"
      (html '(:a :href "/x/" :rel "me" "go")))
  (is string= "<meta http-equiv=\"refresh\">
" (html '(:meta :http-equiv "refresh"))
      "Keyword hyphens are kept in attribute names.")
  (is string= "<span>x</span>" (html '(:span :class nil "x"))
      "A NIL value omits the attribute.")
  (is string= "<input hidden>" (html '(:input :hidden t))
      "A T value writes a boolean attribute name."))

(define-test (serialize escaping)
  (is string= "<p>a &lt; b &amp;&amp; c &gt; d</p>
" (html '(:p "a < b && c > d")))
  (is string= "<a title=\"&quot;quoted&quot;\">x</a>"
      (html '(:a :title "\"quoted\"" "x"))
      "Attribute escaping protects the quote that would end the value.")
  (is string= "<p><em>markup</em></p>
" (html '(:p (:raw "<em>markup</em>")))
      "Raw content reaches the output unescaped."))

(define-test (serialize void-elements)
  (is string= "<br>" (html '(:br)))
  (is string= "<img src=\"a.png\" alt=\"A\">" (html '(:img :src "a.png" :alt "A")))
  (fail (html '(:br "child"))
      'error "A void element cannot have children."))

(define-test (serialize form-lists)
  (is string= "<p>a</p>
<p>b</p>
" (html '((:p "a") (:p "b")))
      "A bare list of forms serializes in order. Head-tag composition returns
this shape.")
  (is string= "<p>ac</p>
" (html '(:p "a" nil "c"))
      "A NIL child writes nothing. A layout can drop an element in place."))
