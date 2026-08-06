;;;; The site graph.
;;;;
;;;; Built once, after every document is parsed and before anything is
;;;; rendered, because a page cannot be laid out until the questions it asks
;;;; about its neighbours can be answered.
;;;;
;;;; The relations here are the ones something currently renders: chronology
;;;; and sections. Taxonomy arrives with the /tags/ page and the reference
;;;; relation arrives with check, for the same reason conditions.lisp holds
;;;; only the conditions the build can signal. A graph that answers questions
;;;; nobody asks is a claim the code does not back.

(in-package #:anchor)

(defstruct (site (:constructor %make-site))
  "Every page, and the relations between them."
  root
  pages
  chronology
  pages-by-url
  children-by-url
  configs)

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

(defun make-site (root &key drafts)
  "Parse every page under ROOT and build the graph.
Drafts are left out of the graph rather than only out of the output, so a
published page linking to one is a broken reference and will be reported as
one rather than quietly producing a dead link."
  (let* ((root (resolve-root root))
         (pages (if drafts
                    (discover-pages root)
                    (remove-if #'page-draft-p (discover-pages root))))
         (pages-by-url (make-hash-table :test #'equal))
         (children-by-url (make-hash-table :test #'equal)))
    (dolist (page pages)
      (setf (gethash (page-url page) pages-by-url) page))
    (let ((site (%make-site :root root
                            :pages pages
                            :chronology (sort-chronologically pages)
                            :pages-by-url pages-by-url
                            :children-by-url children-by-url
                            :configs (directory-configs root))))
      (dolist (page pages)
        (let ((parent (site-parent site page)))
          (when parent
            (push page (gethash (page-url parent) children-by-url)))))
      ;; Dated children in date order, then the undated ones in the order
      ;; they were found. A section with no dates still lists predictably.
      (maphash (lambda (url children)
                 (let ((children (nreverse children)))
                   (setf (gethash url children-by-url)
                         (append (sort-chronologically children)
                                 (remove-if #'page-date children)))))
               children-by-url)
      site)))
