;;;; Build the site graph.
;;;;
;;;; This file is separate from the SITE type and its query functions because
;;;; reference resolution needs both sides.
;;;;
;;;; Reference resolution asks the graph whether a target is a page, so it must
;;;; be compiled after the query functions. The graph is not complete until
;;;; references are resolved. This file is where those two steps meet.

(in-package #:anchor)

(defun make-site (root &key drafts)
  "Parse pages below ROOT and build the site graph.

Drafts are excluded from the graph, not only from output. A published page
that links to a draft is therefore a broken reference, and anchor reports it
as one instead of silently producing a dead link."
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
      ;; Put dated children first, most recent first. Keep undated children in
      ;; discovery order. A section with no dates still lists predictably.
      (maphash (lambda (url children)
                 (let ((children (nreverse children)))
                   (setf (gethash url children-by-url)
                         (append (sort-chronologically children)
                                 (remove-if #'page-date children)))))
               children-by-url)
      ;; Resolve references last. Reference resolution asks whether a target is
      ;; a page, and it rewrites the document tree before rendering starts.
      (setf (site-references site) (resolve-references site))
      site)))
