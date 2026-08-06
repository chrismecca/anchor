;;;; Building a site.
;;;;
;;;; Source tree in, output tree out. Assets are copied into the output rather
;;;; than served from the source, so that the output directory is
;;;; self-contained and "orphan" has an unambiguous meaning later.
;;;;
;;;; This is the walking skeleton: every page is rendered with the one layout
;;;; there is. The site graph, the configuration cascade and the emitters are
;;;; not here yet, which is why BUILD passes NIL where a site will go.

(in-package #:anchor)

(defun page-output-path (page output)
  "Where PAGE is written under OUTPUT.
A URL of \"/posts/2026/first/\" becomes \"posts/2026/first/index.html\", and
the root page becomes \"index.html\"."
  (merge-pathnames (concatenate 'string (subseq (page-url page) 1) "index.html")
                   output))

(defun page-layout (page site)
  "The layout function for PAGE.
The :layout key from the cascade when there is one, named either as a symbol
or as a function. Otherwise the root page gets the front page and everything
else the article, so that a site with no _anchor.lisp at all still produces
an index rather than nothing to click."
  (let ((configured (getf (site-config site page) :layout)))
    (cond ((functionp configured) configured)
          (configured (fdefinition configured))
          ((string= (page-url page) "/") #'front-page)
          (t #'article))))

(defun render-page (page site)
  "PAGE as a complete HTML document."
  (render-document
   (page-shell page site (funcall (page-layout page site) page site))))

(defun write-page (page output &optional site)
  "Write PAGE into OUTPUT and return the path written."
  (let ((path (page-output-path page output)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :external-format :utf-8)
      (write-string (render-page page site) stream))
    path))

(defparameter *policy* :interactive
  "What a build does about a problem.

:strict:: Any condition aborts. This is what CI wants.
:permissive:: Report it and carry on. This is what --force asks for.
:interactive:: Signal it with a restart established, so a REPL stops at the
  offending page with the state still live and a person decides. From a
  script, where there is no debugger, this aborts like :strict.

:interactive is the default because it is the reason anchor is written in
Lisp rather than something else.")

(defun report-problem (condition)
  "Raise CONDITION according to *POLICY*."
  (ecase *policy*
    (:strict (error condition))
    (:permissive (warn "~a" condition))
    (:interactive
     (with-simple-restart (continue "Leave it as written and go on.")
       (error condition)))))

(defun report-missing-references (site)
  "Raise a MISSING-REFERENCE for each target in SITE that resolved to nothing.
Resolution only classifies. This is where a broken reference becomes a
problem, and *POLICY* is what decides how much of one."
  (dolist (reference (missing-references site))
    (report-problem
     (make-condition 'missing-reference
                     :reference reference
                     :url (page-url (reference-page reference))
                     :target (reference-target reference)))))

(defun write-output (file output)
  "Write an emitter's FILE into OUTPUT and return the path written."
  (let ((path (merge-pathnames (output-path file) output)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :external-format :utf-8)
      (write-string (output-content file) stream))
    path))

(defun asset-files (root)
  "Every file under ROOT that is copied to the output verbatim.
Documents are not assets: an .adoc file is either a page, which is rendered,
or an include, which belongs to the document that includes it. Reserved names
are skipped along with the directories holding them."
  (loop for directory in (content-directories root)
        append (loop for file in (uiop:directory-files directory)
                     for name = (file-namestring file)
                     unless (or (reserved-name-p name)
                                (string-equal "adoc" (pathname-type file)))
                       collect file)))

(defun site-asset-files (site)
  "Every file SITE writes to the output verbatim.
Ordinary assets are copied whether or not anything points at them, which is
why an unreferenced one is something check reports rather than something the
build silently drops. Reserved directories are the exception: _media/ holds
shared assets for a whole subtree and only the referenced ones are copied,
so adding a file there costs nothing until a page names it."
  (let ((root (site-root site)))
    (append (asset-files root)
            (remove-if-not (lambda (path)
                             (reserved-path-p path root))
                           (referenced-assets site)))))

(defun reserved-path-p (path root)
  "Whether PATH lies under a reserved directory of ROOT."
  (let ((relative (uiop:enough-pathname path root)))
    (some #'reserved-name-p (rest (pathname-directory relative)))))

(defun copy-assets (site output)
  "Copy SITE's assets into OUTPUT and return the paths written."
  (let ((root (site-root site)))
    (loop for file in (site-asset-files site)
          for target = (merge-pathnames (uiop:enough-pathname file root) output)
          do (ensure-directories-exist target)
             (uiop:copy-file file target)
          collect target)))

(defun build (root &key output drafts (policy *policy*))
  "Build the site rooted at ROOT. Returns the site that was built.
OUTPUT defaults to _site/ under ROOT, which discovery skips because it is a
reserved name, so a build never reads its own output. DRAFTS includes pages
carrying :draft:, which is what serve does and what --drafts asks for. POLICY
is bound for the whole build, so one setting governs every problem it hits."
  (let* ((*policy* policy)
         (site (make-site root :drafts drafts))
         (root (site-root site))
         ;; The output directory is not resolved with the root's TRUENAME,
         ;; since it usually does not exist until the first page is written.
         (output (uiop:ensure-absolute-pathname
                  (uiop:ensure-directory-pathname
                   (or output (merge-pathnames "_site/" root)))
                  (uiop:getcwd))))
    (report-missing-references site)
    (dolist (page (site-pages site))
      (write-page page output site))
    (dolist (file (emit site))
      (write-output file output))
    (copy-assets site output)
    site))
