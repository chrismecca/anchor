;;;; The reference relation.
;;;;
;;;; Every link and image in every document is resolved once, after the graph
;;;; exists and before anything renders.
;;;;
;;;; This is the validation step of the pipeline. It is also all of media
;;;; management. "What this page points at" and "what points at this file" are
;;;; the same relation read in different directions.
;;;;
;;;; A relative target resolves by walking from the page directory upward to the
;;;; site root. anchor tries the target under each directory.
;;;;
;;;; Page-local media is found first. _media/ assets are found later. Colocated
;;;; media and shared media therefore use one rule, not two. It is the same
;;;; cascade used by configuration. anchor has only one search rule.
;;;;
;;;; A resolved target is written back into the cl-adoc tree. anchor renders
;;;; through cl-adoc, and cl-adoc reads the target from the node. Rewriting the
;;;; node is therefore the only way to change the output without rewriting
;;;; rendered HTML.
;;;;
;;;; anchor does not rewrite rendered HTML.

(in-package #:anchor)

(defstruct (reference (:constructor %make-reference))
  "One link or image and its resolved target."
  page
  node
  target
  kind
  path
  url)

(defun set-reference-target (node target)
  "Make NODE point at TARGET.

cl-adoc has REFERENCE-TARGET for reading, but no SETF form for it. The write
therefore goes through the three exported accessors it dispatches over.

A future cl-adoc (SETF REFERENCE-TARGET) would be useful. This is not a bug
workaround. It is only an affordance that does not exist yet."
  (etypecase node
    (adoc:link (setf (adoc:link-target node) target))
    (adoc:inline-image (setf (adoc:inline-image-target node) target))
    (adoc:block-image (setf (adoc:block-image-target node) target))))

(defun url-scheme-p (target)
  "Return true when TARGET names a location outside this site."
  (or (and (>= (length target) 2) (string= "//" target :end2 2))
      (let ((colon (position #\: target)))
        (and colon
             (plusp colon)
             (every (lambda (character)
                      (or (alphanumericp character) (find character "+-.")))
                    (subseq target 0 colon))))))

(defun split-target (target)
  "Return two values: the part of TARGET that names a file or page, and the rest.

The rest is a query, a fragment, or both. It starts at the first ? or #. It
takes no part in resolution and is put back after the resolved URL, so
../about/#team resolves ../about/ and is rewritten to /about/#team."
  (let ((end (position-if (lambda (character) (find character "?#")) target)))
    (if end
        (values (subseq target 0 end) (subseq target end))
        (values target ""))))

(defun target-pathname (target)
  "Parse TARGET as a relative file name.

Characters that Common Lisp reads as wildcards, such as * and [, are taken
literally. A file name in a document is never a pattern."
  (uiop:parse-unix-namestring target))

(defun page-directory (page)
  "Return the directory that contains PAGE's document."
  (uiop:pathname-directory-pathname (page-source page)))

(defun ancestor-directories (directory root)
  "Return DIRECTORY and its ancestors, nearest first.

Stop at ROOT."
  (let ((root-name (namestring root))
        (found '()))
    (loop for dir = directory
            then (uiop:pathname-parent-directory-pathname dir)
          do (push dir found)
             (when (equal (namestring dir) root-name)
               (return))
             ;; A page outside the root would otherwise walk to the filesystem
             ;; root and continue.
             (when (equal (namestring (uiop:pathname-parent-directory-pathname dir))
                          (namestring dir))
               (return)))
    (nreverse found)))

(defun asset-url (site path)
  "Return the URL that asset PATH will have in SITE's output."
  (concatenate 'string "/"
               ;; NAMESTRING would write a * in a file name as \*.
               (uiop:native-namestring (uiop:enough-pathname path (site-root site)))))

(defun url-segments (url)
  (remove "" (uiop:split-string url :separator "/") :test #'string=))

(defun merge-url (base relative)
  "Resolve RELATIVE against directory URL BASE."
  (let ((segments (url-segments base)))
    (dolist (piece (url-segments relative))
      (cond ((string= piece "."))
            ((string= piece "..") (setf segments (butlast segments)))
            (t (setf segments (append segments (list piece))))))
    (format nil "/~{~a/~}" segments)))

(defun find-referenced-file (site page target)
  "Search for TARGET as a file.

Start at PAGE's directory and walk upward to SITE's root."
  (loop for directory in (ancestor-directories (page-directory page)
                                               (site-root site))
        for candidate = (probe-file (merge-pathnames (target-pathname target)
                                                     directory))
        when (and candidate (not (uiop:directory-pathname-p candidate)))
          return candidate))

(defun resolve-reference (site page node)
  "Classify what NODE on PAGE points at."
  (let ((target (adoc:reference-target node)))
    (multiple-value-bind (name suffix) (split-target (or target ""))
      (flet ((found (kind &key path url)
               (%make-reference :page page :node node :target target
                                :kind kind :path path
                                :url (when url (concatenate 'string url suffix)))))
        (cond
          ((or (null target) (string= target "")) (found :missing))
          ;; Only a query or a fragment. It points into this page.
          ((string= name "") (found :fragment))
          ((url-scheme-p target) (found :external))
          ((char= (char name 0) #\/)
           (let ((file (probe-file (merge-pathnames (target-pathname (subseq name 1))
                                                    (site-root site)))))
             (cond ((site-page site name) (found :page :url name))
                   ((and file (not (uiop:directory-pathname-p file)))
                    (found :asset :path file :url name))
                   (t (found :missing)))))
          (t
           (let ((file (find-referenced-file site page name)))
             (if file
                 (found :asset :path file :url (asset-url site file))
                 (let ((url (merge-url (page-url page) name)))
                   (if (site-page site url)
                       (found :page :url url)
                       (found :missing)))))))))))

(defun resolve-references (site)
  "Resolve every reference in SITE.

Rewrite resolved targets in the document tree.

This function only classifies references. The build decides whether a missing
reference stops the build. check must be able to report a site that still has
missing references."
  (loop for page in (site-pages site)
        append (loop for node in (adoc:document-references (page-document page))
                     for reference = (resolve-reference site page node)
                     do (when (reference-url reference)
                          ;; Use a site-absolute URL. Then the page can be
                          ;; served from any depth, and a shared asset has the
                          ;; same URL from every page that names it.
                          (set-reference-target node (reference-url reference)))
                     collect reference)))

(defun missing-references (site)
  "Return every reference in SITE that resolved to nothing."
  (remove :missing (site-references site) :key #'reference-kind :test-not #'eq))

(defun referenced-assets (site)
  "Return files referenced by SITE's documents, without duplicates."
  (remove-duplicates
   (loop for reference in (site-references site)
         when (eq :asset (reference-kind reference))
           collect (reference-path reference))
   :test #'equal))
