;;;; XML serialization tests.

(in-package #:anchor/tests)

(defun xml (form)
  (anchor:serialize-xml form))

(define-test serialize-xml)

(define-test (serialize-xml xml-elements)
  (is string= "<title>text</title>" (xml '(:title "text"))
      "An element that contains only text stays on one line.")
  (is string= "<link/>" (xml '(:link))
      "An empty element closes itself. XML has no void tags, so there is no
exception list to maintain.")
  (is string= "<link rel=\"self\" href=\"/feed.xml\"/>"
      (xml '(:link :rel "self" :href "/feed.xml"))))

(define-test (serialize-xml nesting)
  (is string= "<feed>
<title>A Site</title>
<id>x</id>
</feed>"
      (xml '(:feed (:title "A Site") (:id "x")))
      "An element that contains elements writes each child on its own line.")
  (is string= "<feed>
<title>A Site</title>
</feed>"
      (xml '(:feed (:title "A Site") nil))
      "A NIL child is dropped. A layout can omit an element in place."))

(define-test (serialize-xml xml-escaping)
  (is string= "<summary>a &lt; b &amp;&amp; c &gt; d</summary>"
      (xml '(:summary "a < b && c > d")))
  (is string= "<summary>&lt;p&gt;markup&lt;/p&gt;</summary>"
      (xml '(:summary "<p>markup</p>"))
      "HTML inside an XML text node is escaped. The feed depends on this when
it carries an excerpt.")
  (is string= "<link href=\"?a=1&amp;b=2\"/>"
      (xml '(:link :href "?a=1&b=2"))))
