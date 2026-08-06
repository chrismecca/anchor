;;;; Serialising XML trees.

(in-package #:anchor/tests)

(defun xml (form)
  (anchor:serialize-xml form))

(define-test serialize-xml)

(define-test (serialize-xml elements)
  (is string= "<title>text</title>" (xml '(:title "text"))
      "An element holding only text stays on one line.")
  (is string= "<link/>" (xml '(:link))
      "An element with nothing in it closes itself. XML has no void tags, so
there is no list of exceptions to keep in step with.")
  (is string= "<link rel=\"self\" href=\"/feed.xml\"/>"
      (xml '(:link :rel "self" :href "/feed.xml"))))

(define-test (serialize-xml nesting)
  (is string= "<feed>
<title>A Site</title>
<id>x</id>
</feed>"
      (xml '(:feed (:title "A Site") (:id "x")))
      "An element holding other elements puts each on its own line.")
  (is string= "<feed>
<title>A Site</title>
</feed>"
      (xml '(:feed (:title "A Site") nil))
      "A NIL child is dropped rather than left as a blank line, so a layout
can omit an element in place."))

(define-test (serialize-xml escaping)
  (is string= "<summary>a &lt; b &amp;&amp; c &gt; d</summary>"
      (xml '(:summary "a < b && c > d")))
  (is string= "<summary>&lt;p&gt;markup&lt;/p&gt;</summary>"
      (xml '(:summary "<p>markup</p>"))
      "HTML placed in an XML text node is escaped, which is what carrying an
excerpt through a feed depends on.")
  (is string= "<link href=\"?a=1&amp;b=2\"/>"
      (xml '(:link :href "?a=1&b=2"))))
