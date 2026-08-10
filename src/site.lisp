;;;; The site and its queries.
;;;;
;;;; The graph is built once, after every document is parsed and before any
;;;; page renders.
;;;;
;;;; A page cannot be laid out until anchor can answer questions about its
;;;; neighbors. graph.lisp builds those relations. This file defines the SITE
;;;; type and the functions that read it.
;;;;
;;;; The relations here are the ones rendering code uses: chronology, sections,
;;;; taxonomy, configuration, and references.
;;;;
;;;; A graph relation that no code reads is only a claim. This is the same rule
;;;; conditions.lisp follows: define only what current code can signal.

(in-package #:anchor)

(defstruct (site (:constructor %make-site))
  "A site graph.

Stores every page and the relations between pages, assets, tags, and
configuration."
  root
  pages
  chronology
  pages-by-url
  children-by-url
  pages-by-tag
  configs
  references)

(defun parent-url (url)
  "Return the URL one level above URL.

Return NIL at the root."
  (unless (string= url "/")
    ;; Exclude the trailing slash from the search. The slash found is therefore
    ;; the one before this directory's own name.
    (let ((slash (position #\/ url :from-end t :end (1- (length url)))))
      (subseq url 0 (1+ slash)))))

(defun sort-chronologically (pages)
  "Return the dated members of PAGES, most recent first.

ISO 8601 dates sort correctly as strings. That is why the specification asks
for that format."
  (sort (remove-if-not #'page-date (copy-list pages)) #'string> :key #'page-date))

(defun site-page (site url)
  "Return the page at URL.

Return NIL when SITE has no page at URL."
  (gethash url (site-pages-by-url site)))

(defun site-parent (site page)
  "Return the nearest ancestor of PAGE that is also a page.

A directory without index.adoc is a path component, not a section. Thus,
posts/2026/first-post/ has posts/ as its parent when posts/2026/ has no
document."
  (loop for url = (parent-url (page-url page)) then (parent-url url)
        while url
        for candidate = (site-page site url)
        when candidate return candidate))

(defun site-children (site page)
  "Return PAGE's child pages, most recent first.

A child page is a page whose nearest page ancestor is PAGE."
  (gethash (page-url page) (site-children-by-url site)))

(defun site-tags (site)
  "Return every tag used by SITE, alphabetically."
  (sort (loop for tag being the hash-keys of (site-pages-by-tag site)
              collect tag)
        #'string-lessp))

(defun site-tagged (site tag)
  "Return pages tagged with TAG, most recent first."
  (gethash tag (site-pages-by-tag site)))

(defun site-root-config (site)
  "Return the root configuration.

Generated pages use this configuration."
  (gethash (namestring (site-root site)) (site-configs site)))

(defun base-url (site)
  "Return SITE's :base-url without a trailing slash.

Return NIL when SITE has no base URL.

This function lives here because emitters and head tags both need it.
Canonical links, Open Graph URLs, and feed entry IDs all need absolute URLs."
  (let ((base (getf (site-root-config site) :base-url)))
    (when base (string-right-trim "/" base))))

(defun absolute-url (site url)
  "Return URL made absolute against SITE's base URL.

Return NIL when SITE has no base URL."
  (let ((base (base-url site)))
    (when base (concatenate 'string base url))))

(defun site-config (site page)
  "Return the cascaded configuration in force for PAGE.

A NIL SITE has no configuration. This lets a layout be called during
development without special-casing the call."
  (when site
    (gethash (namestring (uiop:pathname-directory-pathname (page-source page)))
             (site-configs site))))

(defun undated-in-dated-section (site)
  "Return pages that should have a date but do not.

A page needs a date when every other page in its section has one. A dated
section is therefore inferred from sibling pages, not declared in
configuration.

This keeps mixed sites quiet. A root can contain a dated posts/ section and an
undated about/ section. about/ is not asked for a date.

A posts/ section where every entry except the newest has a date is different.
That is the moment a date was probably forgotten."
  (loop for page in (site-pages site)
        for parent = (site-parent site page)
        when (and (null (page-date page)) parent)
          append (let ((siblings (remove page (site-children site parent))))
                   (when (and siblings (every #'page-date siblings))
                     (list page)))))

(defun site-ancestors (site page)
  "Return PAGE's ancestors, nearest first."
  (loop for parent = (site-parent site page) then (site-parent site parent)
        while parent
        collect parent))
