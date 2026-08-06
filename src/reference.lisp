;;;; The reference relation.
;;;;
;;;; Every link and image in every document, resolved once, after the graph
;;;; exists and before anything renders. This is the validate step of the
;;;; pipeline and it is the whole of media management: what a page points at,
;;;; and what points at a given file, are the same relation read in the two
;;;; directions.
;;;;
;;;; Resolving a relative target walks up from the page's own directory to the
;;;; root, trying the target under each. Page-local media is found on the
;;;; first step and _media/ is found on a later one, so colocation and shared
;;;; assets are not two rules. It is the cascade the configuration uses, which
;;;; is the only searching rule anchor has.
;;;;
;;;; A resolved target is written back into the tree. anchor renders through
;;;; cl-adoc, which reads the target off the node, so rewriting the node is
;;;; the only way to change the output without a pass over the rendered HTML
;;;; -- and a pass over rendered HTML is the thing this project exists to not
;;;; do.

(in-package #:anchor)

(defstruct (reference (:constructor %make-reference))
  "One link or image, and what it turned out to point at."
  page
  node
  target
  kind
  path
  url)

(defun set-reference-target (node target)
  "Point NODE at TARGET.
cl-adoc's REFERENCE-TARGET reads but does not write, so the write goes
through the three exported accessors it dispatches over. Worth an upstream
(setf reference-target) eventually; this is not a workaround for a bug, only
for an affordance that is not there yet."
  (etypecase node
    (adoc:link (setf (adoc:link-target node) target))
    (adoc:inline-image (setf (adoc:inline-image-target node) target))
    (adoc:block-image (setf (adoc:block-image-target node) target))))

(defun url-scheme-p (target)
  "Whether TARGET names somewhere outside this site."
  (or (and (>= (length target) 2) (string= "//" target :end2 2))
      (let ((colon (position #\: target)))
        (and colon
             (plusp colon)
             (every (lambda (character)
                      (or (alphanumericp character) (find character "+-.")))
                    (subseq target 0 colon))))))

(defun page-directory (page)
  "The directory PAGE's document lives in."
  (uiop:pathname-directory-pathname (page-source page)))

(defun ancestor-directories (directory root)
  "DIRECTORY and its ancestors, nearest first, stopping at ROOT."
  (let ((root-name (namestring root))
        (found '()))
    (loop for dir = directory
            then (uiop:pathname-parent-directory-pathname dir)
          do (push dir found)
             (when (equal (namestring dir) root-name)
               (return))
             ;; A page outside the root would otherwise walk to the filesystem
             ;; root and keep going.
             (when (equal (namestring (uiop:pathname-parent-directory-pathname dir))
                          (namestring dir))
               (return)))
    (nreverse found)))

(defun asset-url (site path)
  "The URL an asset at PATH will have once copied into the output."
  (concatenate 'string "/"
               (namestring (uiop:enough-pathname path (site-root site)))))

(defun url-segments (url)
  (remove "" (uiop:split-string url :separator "/") :test #'string=))

(defun merge-url (base relative)
  "RELATIVE resolved against the directory URL BASE."
  (let ((segments (url-segments base)))
    (dolist (piece (url-segments relative))
      (cond ((string= piece "."))
            ((string= piece "..") (setf segments (butlast segments)))
            (t (setf segments (append segments (list piece))))))
    (format nil "/~{~a/~}" segments)))

(defun find-referenced-file (site page target)
  "TARGET as a file, searched from PAGE's directory upwards to the root."
  (loop for directory in (ancestor-directories (page-directory page)
                                               (site-root site))
        for candidate = (probe-file (merge-pathnames target directory))
        when (and candidate (not (uiop:directory-pathname-p candidate)))
          return candidate))

(defun resolve-reference (site page node)
  "Classify what NODE on PAGE points at."
  (let ((target (adoc:reference-target node)))
    (flet ((found (kind &key path url)
             (%make-reference :page page :node node :target target
                              :kind kind :path path :url url)))
      (cond
        ((or (null target) (string= target "")) (found :missing))
        ((char= (char target 0) #\#) (found :fragment))
        ((url-scheme-p target) (found :external))
        ((char= (char target 0) #\/)
         (let ((file (probe-file (merge-pathnames (subseq target 1)
                                                  (site-root site)))))
           (cond ((site-page site target) (found :page :url target))
                 ((and file (not (uiop:directory-pathname-p file)))
                  (found :asset :path file :url target))
                 (t (found :missing)))))
        (t
         (let ((file (find-referenced-file site page target)))
           (if file
               (found :asset :path file :url (asset-url site file))
               (let ((url (merge-url (page-url page) target)))
                 (if (site-page site url)
                     (found :page :url url)
                     (found :missing))))))))))

(defun resolve-references (site)
  "Resolve every reference in SITE, rewriting the resolved ones in the tree.
Classification only. Whether a missing reference stops the build is the
build's decision, and check has to be able to report on a site that has some."
  (loop for page in (site-pages site)
        append (loop for node in (adoc:document-references (page-document page))
                     for reference = (resolve-reference site page node)
                     do (when (reference-url reference)
                          ;; Site-absolute, so the page can be served from any
                          ;; depth and a shared asset resolves the same from
                          ;; every page that names it.
                          (set-reference-target node (reference-url reference)))
                     collect reference)))

(defun missing-references (site)
  "Every reference in SITE that resolved to nothing."
  (remove :missing (site-references site) :key #'reference-kind :test-not #'eq))

(defun referenced-assets (site)
  "The files SITE's documents point at, without duplicates."
  (remove-duplicates
   (loop for reference in (site-references site)
         when (eq :asset (reference-kind reference))
           collect (reference-path reference))
   :test #'equal))
