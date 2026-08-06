;;;; Pages, and finding them.
;;;;
;;;; The filesystem is the data model. A directory holding an index.adoc is a
;;;; page and its URL is its path, so discovery is a walk and there is nothing
;;;; to configure.
;;;;
;;;; A page stores its source, its URL and its parsed document, and nothing
;;;; else. Title and date are read back out of the document every time they
;;;; are asked for rather than copied into slots here, because the moment
;;;; anchor keeps its own copy of something the document already knows, the
;;;; two can disagree.

(in-package #:anchor)

(defstruct page
  "A document that will be written to the output tree at a URL."
  source
  url
  document)

(defun resolve-root (root)
  "ROOT as an absolute, resolved directory pathname.
A page's URL is its path relative to the root, and that subtraction needs
both sides in the same form. Merely making the path absolute is not enough:
a root of \".\" keeps its dot as a directory component, which then fails to
match the prefix of any directory beneath it. TRUENAME resolves the dot, and
signals plainly when the root does not exist."
  (truename (uiop:ensure-directory-pathname root)))

(defun reserved-name-p (name)
  "Whether NAME is reserved. Anything beginning with an underscore is."
  (and (plusp (length name)) (char= (char name 0) #\_)))

(defun directory-name (path)
  "The name of PATH's own directory, without its parents."
  (car (last (pathname-directory path))))

(defun content-directories (root)
  "ROOT and every directory beneath it whose name is not reserved."
  (cons root
        (loop for sub in (uiop:subdirectories root)
              unless (reserved-name-p (directory-name sub))
                append (content-directories sub))))

(defun directory-url (root directory)
  "DIRECTORY's path relative to ROOT, as a URL with a trailing slash."
  (let ((relative (uiop:enough-pathname directory root)))
    (format nil "/~{~a/~}" (rest (pathname-directory relative)))))

(defun parse-document (path)
  "Parse PATH, resignalling a cl-adoc failure as UNREADABLE-DOCUMENT.
anchor does not report cl-adoc's conditions as its own, so that a caller
handling anchor's build errors does not have to know what parses the input."
  (handler-case (adoc:parse-file path)
    (adoc:adoc-error (cause)
      (error 'unreadable-document :path path :cause cause))))

(defun discover-pages (root)
  "Every page under ROOT, in directory order.
An .adoc file not named index.adoc is not a page; it is there to be included
by a document that wants it. A SKIP-PAGE restart is offered around each page
so that one unparseable document does not end the build."
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
  "PAGE's title as text, or NIL when the document has none."
  (let ((title (adoc:document-title (page-document page))))
    (and title (adoc:node-text title))))

(defun page-date (page)
  "PAGE's :date: attribute, or NIL when it has none."
  (adoc:document-attribute (page-document page) "date"))

(defparameter *month-names*
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun page-date-human (page)
  "PAGE's date as prose, or NIL when it has none.
A date anchor cannot read is returned unchanged rather than replaced or
dropped: showing the author what they actually wrote is more use than hiding
it behind a guess."
  (let ((date (page-date page)))
    (when date
      (or (ignore-errors
           (let ((year (parse-integer date :start 0 :end 4))
                 (month (parse-integer date :start 5 :end 7))
                 (day (parse-integer date :start 8 :end 10)))
             (format nil "~a ~d, ~d" (aref *month-names* (1- month)) day year)))
          date))))

(defun split-list (string)
  "STRING split on commas, trimmed, with the empty pieces dropped."
  (loop for start = 0 then (1+ comma)
        for comma = (position #\, string :start start)
        for piece = (string-trim '(#\Space #\Tab)
                                 (subseq string start (or comma (length string))))
        unless (string= piece "") collect piece
          while comma))

(defun page-tags (page)
  "PAGE's tags, in the order the document gives them."
  (let ((tags (adoc:document-attribute (page-document page) "tags")))
    (when tags (split-list tags))))

(defun page-draft-p (page)
  "Whether PAGE carries :draft:.
The attribute's presence is what counts and its value is never read, so
`:draft: false` is still a draft. Deleting the line is how a draft is
published. One rule, and no table of what counts as false."
  (and (adoc:document-attribute (page-document page) "draft") t))

(defun render-blocks (blocks)
  "BLOCKS rendered as an HTML fragment."
  (let ((backend (make-instance 'adoc:html5)))
    (with-output-to-string (out)
      (dolist (block blocks)
        (adoc:render-node block backend out)))))

(defun page-body-html (page)
  "PAGE's blocks rendered as HTML, without its title.
cl-adoc's RENDER-HTML emits the document title as an h1, which a layout
supplies itself from PAGE-TITLE, so the blocks are rendered one at a time
here instead."
  (render-blocks (adoc:document-blocks (page-document page))))

(defun page-preamble (page)
  "PAGE's blocks up to its first heading."
  (loop for block in (adoc:document-blocks (page-document page))
        until (adoc:heading-p block)
        collect block))

(defun page-excerpt-html (page)
  "PAGE's preamble as HTML, or its first paragraph when it has no preamble.
This is a subtree of the parsed document rendered on its own. There is no
marker in the source, no attribute, and nothing truncates rendered HTML."
  (render-blocks
   (or (page-preamble page)
       (let ((paragraph (adoc:find-node #'adoc:paragraph-p (page-document page))))
         (and paragraph (list paragraph))))))
