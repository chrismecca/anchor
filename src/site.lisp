;;;; The site, and the questions it answers.
;;;;
;;;; The graph is built once, after every document is parsed and before
;;;; anything is rendered, because a page cannot be laid out until the
;;;; questions it asks about its neighbours can be answered. Building it is in
;;;; graph.lisp; this file is the type and the reading.
;;;;
;;;; The relations here are the ones something renders: chronology, sections,
;;;; and taxonomy now that there is a /tags/ page to consume it. A graph that
;;;; answers questions nobody asks is a claim the code does not back, which is
;;;; the same reason conditions.lisp holds only the conditions something can
;;;; signal.

(in-package #:anchor)

(defstruct (site (:constructor %make-site))
  "Every page, and the relations between them."
  root
  pages
  chronology
  pages-by-url
  children-by-url
  pages-by-tag
  configs
  references)

(defun parent-url (url)
  "The URL one level above URL, or NIL at the root."
  (unless (string= url "/")
    ;; The trailing slash is excluded from the search, so the slash that is
    ;; found is the one before this directory's own name.
    (let ((slash (position #\/ url :from-end t :end (1- (length url)))))
      (subseq url 0 (1+ slash)))))

(defun sort-chronologically (pages)
  "The dated members of PAGES, most recent first.
ISO 8601 dates compare correctly as strings, which is most of why the spec
asks for that format and not another."
  (sort (remove-if-not #'page-date (copy-list pages)) #'string> :key #'page-date))

(defun site-page (site url)
  "The page at URL, or NIL when there is none."
  (gethash url (site-pages-by-url site)))

(defun site-parent (site page)
  "The nearest ancestor of PAGE that is itself a page.
A directory that holds no index.adoc is a path component and not a section,
so posts/2026/first-post/ has posts/ for a parent when posts/2026/ has no
document of its own."
  (loop for url = (parent-url (page-url page)) then (parent-url url)
        while url
        for candidate = (site-page site url)
        when candidate return candidate))

(defun site-children (site page)
  "The pages whose nearest page-ancestor is PAGE, most recent first."
  (gethash (page-url page) (site-children-by-url site)))

(defun site-tags (site)
  "Every tag in use, alphabetically."
  (sort (loop for tag being the hash-keys of (site-pages-by-tag site)
              collect tag)
        #'string-lessp))

(defun site-tagged (site tag)
  "The pages carrying TAG, most recent first."
  (gethash tag (site-pages-by-tag site)))

(defun site-root-config (site)
  "The configuration at the root, which is what applies to a generated page."
  (gethash (namestring (site-root site)) (site-configs site)))

(defun site-config (site page)
  "The configuration in force for PAGE: its own directory's, cascaded.
A NIL site has no configuration, so a layout can be called with one during
development without special-casing the call."
  (when site
    (gethash (namestring (uiop:pathname-directory-pathname (page-source page)))
             (site-configs site))))

(defun site-ancestors (site page)
  "PAGE's ancestors, nearest first."
  (loop for parent = (site-parent site page) then (site-parent site parent)
        while parent
        collect parent))
