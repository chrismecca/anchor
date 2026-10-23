;;;; Pages and page discovery.
;;;;
;;;; The filesystem is the data model. A directory that contains index.adoc is
;;;; a page. Its path is its URL. Discovery is therefore a filesystem walk, and
;;;; there is nothing to configure.
;;;;
;;;; A PAGE stores its source path, URL, and parsed document. It stores nothing
;;;; else.
;;;;
;;;; Title and date are read from the document each time they are requested.
;;;; They are not copied into page slots. When anchor keeps a second copy of
;;;; data the document already knows, the two copies can disagree.

(in-package #:anchor)

(defstruct page
  "A document that will be written to the output tree at a URL."
  source
  url
  document)

(defun resolve-root (root)
  "Return ROOT as an absolute, resolved directory pathname.

A page URL is the page path relative to ROOT. That subtraction needs both
pathnames in the same form.

Making the pathname absolute is not enough. A root of \".\" keeps the dot as a
directory component, which then fails to match the prefix of directories below
it.

TRUENAME resolves the dot. It also signals clearly when ROOT does not exist."
  (truename (uiop:ensure-directory-pathname root)))

(defun reserved-name-p (name)
  "Return true when NAME is reserved.

A name is reserved when it starts with an underscore."
  (and (plusp (length name)) (char= (char name 0) #\_)))

(defun hidden-name-p (name)
  "Return true when NAME is hidden.

A name is hidden when it starts with a dot. Hidden files and directories
belong to other tools, such as .git/ and .DS_Store. anchor does not discover,
copy, check, or watch them.

A site kept in git would otherwise publish its .git/ directory with every
build."
  (and (plusp (length name)) (char= (char name 0) #\.)))

(defun directory-name (path)
  "Return PATH's directory name without its parents."
  (car (last (pathname-directory path))))

(defun content-directories (root)
  "Return ROOT and each directory below it that is neither reserved nor hidden."
  (cons root
        (loop for sub in (uiop:subdirectories root)
              for name = (directory-name sub)
              unless (or (reserved-name-p name) (hidden-name-p name))
                append (content-directories sub))))

(defun directory-url (root directory)
  "Return DIRECTORY's path relative to ROOT as a URL with a trailing slash."
  (let ((relative (uiop:enough-pathname directory root)))
    (format nil "/~{~a/~}" (rest (pathname-directory relative)))))

(defun parse-document (path)
  "Parse PATH and resignal cl-adoc failures as UNREADABLE-DOCUMENT.

anchor does not expose cl-adoc conditions as build errors. A caller that
handles anchor build errors should not need to know which parser reads the
input."
  (handler-case (adoc:parse-file path)
    (adoc:adoc-error (cause)
      (error 'unreadable-document :path path :cause cause))))

(defun discover-pages (root)
  "Return every page below ROOT, in directory order.

An .adoc file not named index.adoc is not a page. It is available for a
document to include, but discovery ignores it.

A SKIP-PAGE restart is offered around each page. One unreadable document does
not have to end the build."
  (loop with root = (resolve-root root)
        for directory in (content-directories root)
        for source = (merge-pathnames "index.adoc" directory)
        when (probe-file source)
          append (with-simple-restart (skip-page "Skip ~a and continue." source)
                   (list (make-page :source source
                                    :url (directory-url root directory)
                                    :document (parse-document source))))))

;;; Reading a page

(defun page-title (page)
  "Return PAGE's title as text.

Return NIL when the document has no title."
  (let ((title (adoc:document-title (page-document page))))
    (and title (adoc:node-text title))))

(defun page-date (page)
  "Return PAGE's :date: attribute.

Return NIL when PAGE has no date."
  (adoc:document-attribute (page-document page) "date"))

(defparameter *month-names*
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun page-date-human (page)
  "Return PAGE's date as prose.

Return NIL when PAGE has no date.

When anchor cannot read the date, return it unchanged. Showing the author what
the document contains is more useful than hiding it behind a guess."
  (let ((date (page-date page)))
    (when date
      (or (ignore-errors
           (let ((year (parse-integer date :start 0 :end 4))
                 (month (parse-integer date :start 5 :end 7))
                 (day (parse-integer date :start 8 :end 10)))
             (format nil "~a ~d, ~d" (aref *month-names* (1- month)) day year)))
          date))))

(defun split-list (string)
  "Split STRING on commas.

Trim each part and drop empty parts."
  (loop for start = 0 then (1+ comma)
        for comma = (position #\, string :start start)
        for piece = (string-trim '(#\Space #\Tab)
                                 (subseq string start (or comma (length string))))
        unless (string= piece "") collect piece
          while comma))

(defun page-tags (page)
  "Return PAGE's tags in document order."
  (let ((tags (adoc:document-attribute (page-document page) "tags")))
    (when tags (split-list tags))))

(defun page-draft-p (page)
  "Return true when PAGE has a :draft: attribute.

Only the attribute's presence matters. Its value is not read. Thus,
`:draft: false` is still a draft. Delete the line to publish the page.

This keeps one rule and avoids a table of false values."
  (and (adoc:document-attribute (page-document page) "draft") t))

(defun render-blocks (blocks)
  "Render BLOCKS as an HTML fragment."
  (let ((backend (make-instance 'adoc:html5)))
    (with-output-to-string (out)
      (dolist (block blocks)
        (adoc:render-node block backend out)))))

(defun page-body-html (page)
  "Return PAGE's body blocks as HTML, without the document title.

cl-adoc's RENDER-HTML emits the document title as an h1. A layout supplies
that h1 from PAGE-TITLE. The body is therefore rendered block by block here."
  (render-blocks (adoc:document-blocks (page-document page))))

(defun page-preamble (page)
  "Return PAGE's blocks up to the first heading."
  (loop for block in (adoc:document-blocks (page-document page))
        until (adoc:heading-p block)
        collect block))

(defun page-uses-stem-p (page)
  "Return true when PAGE contains mathematics.

Ask the document instead of requiring the author to declare the answer. That
is the rule for anything a document can answer about itself.

What a layout does with the answer is the layout's job. anchor ships no math
renderer because it ships no JavaScript."
  (adoc:document-uses-stem-p (page-document page)))

(defun collapse-whitespace (string)
  "Return STRING with whitespace runs reduced to one space."
  (string-trim " "
               (with-output-to-string (out)
                 (let ((pending nil)
                       (started nil))
                   (loop for character across string
                         do (if (member character
                                        '(#\Space #\Tab #\Newline #\Return))
                                (setf pending started)
                                (progn
                                  (when pending (write-char #\Space out))
                                  (setf pending nil started t)
                                  (write-char character out))))))))

(defun truncate-text (string limit)
  "Return STRING cut to LIMIT characters at a word boundary."
  (if (<= (length string) limit)
      string
      (let ((space (position #\Space string :from-end t :end limit)))
        (concatenate 'string (subseq string 0 (or space limit)) "…"))))

(defun page-description (page)
  "Return prose that describes PAGE.

Use PAGE's `:description:` when the document provides one. Otherwise use the
excerpt text.

The document is the metadata. An author-written description wins. Without one,
anchor uses what the page already says."
  (or (adoc:document-attribute (page-document page) "description")
      (let ((text (collapse-whitespace
                   (format nil "~{~a~^ ~}"
                           (mapcar #'adoc:node-text (page-excerpt-blocks page))))))
        (when (plusp (length text))
          (truncate-text text 200)))))

(defun page-excerpt-blocks (page)
  "Return the blocks that make PAGE's excerpt."
  (or (page-preamble page)
      (let ((paragraph (adoc:find-node #'adoc:paragraph-p (page-document page))))
        (and paragraph (list paragraph)))))

(defun page-excerpt-html (page)
  "Return PAGE's excerpt as HTML.

The excerpt is PAGE's preamble, or its first paragraph when it has no preamble.
It is a subtree of the parsed document rendered on its own.

There is no source marker, no attribute, and no truncation of rendered HTML."
  (render-blocks (page-excerpt-blocks page)))
