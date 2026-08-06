;;;; Building the site graph.
;;;;
;;;; Separated from the site type and its queries because resolving references
;;;; needs both: it asks the graph whether a target is a page, so it has to be
;;;; compiled after the queries, and the graph is not finished until it has
;;;; run. This file is where the two meet.

(in-package #:anchor)

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
         (children-by-url (make-hash-table :test #'equal))
         (pages-by-tag (make-hash-table :test #'equal)))
    (dolist (page pages)
      (setf (gethash (page-url page) pages-by-url) page)
      (dolist (tag (page-tags page))
        (push page (gethash tag pages-by-tag))))
    (maphash (lambda (tag tagged)
               (setf (gethash tag pages-by-tag)
                     (let ((tagged (nreverse tagged)))
                       (append (sort-chronologically tagged)
                               (remove-if #'page-date tagged)))))
             pages-by-tag)
    (let ((site (%make-site :root root
                            :pages pages
                            :chronology (sort-chronologically pages)
                            :pages-by-url pages-by-url
                            :children-by-url children-by-url
                            :pages-by-tag pages-by-tag
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
      ;; Last, because resolving a reference asks the graph whether a target
      ;; is a page, and rewriting the tree has to happen before anything is
      ;; rendered from it.
      (setf (site-references site) (resolve-references site))
      site)))
