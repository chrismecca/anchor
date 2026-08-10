;;;; Reference relation tests.

(in-package #:anchor/tests)

(define-test reference)

(defun references-of (site url)
  (remove url (anchor::site-references site)
          :key (lambda (r) (anchor:page-url (anchor::reference-page r)))
          :test-not #'string=))

(defun reference-to (site url target)
  (find target (references-of site url)
        :key #'anchor::reference-target :test #'string=))

(define-test (reference colocated-media)
  ;; A reference keeps the target the document wrote and records the target it
  ;; resolved to. The node is rewritten, so those strings can differ. check can
  ;; then report both what the document wrote and where it resolved.
  (let ((ref (reference-to (test-site) "/posts/2026/first-post/" "diagram.svg")))
    (true ref "A page-local image resolves on the first step of the walk.")
    (is eq :asset (anchor::reference-kind ref))
    (is string= "/posts/2026/first-post/diagram.svg" (anchor::reference-url ref))))

(define-test (reference shared-media)
  (let ((ref (reference-to (test-site) "/posts/2026/second-post/"
                           "_media/portrait.svg")))
    (true ref "_media/ is found later in the same walk, not by a second rule.")
    (is eq :asset (anchor::reference-kind ref))
    (is string= "/_media/portrait.svg" (anchor::reference-url ref))))

(define-test (reference page-links)
  (let* ((site (test-site))
         (absolute (reference-to site "/posts/2026/first-post/"
                                 "/posts/2026/second-post/"))
         (relative (reference-to site "/posts/2026/second-post/"
                                 "../first-post/")))
    (is eq :page (anchor::reference-kind absolute))
    (is eq :page (anchor::reference-kind relative))
    (is string= "/posts/2026/first-post/" (anchor::reference-url relative)
        "A relative link resolves against the referring page's URL. It is then
rewritten site-absolute, so it works from any served depth.")))

(define-test (reference external)
  (let ((ref (reference-to (test-site) "/posts/2026/first-post/"
                           "https://example.org/elsewhere")))
    (is eq :external (anchor::reference-kind ref))
    (false (anchor::reference-url ref)
           "Targets outside the site are not resolved, rewritten, or checked.")))

(define-test (reference rewritten-in-the-tree)
  (let* ((site (test-site))
         (post (anchor:site-page site "/posts/2026/second-post/"))
         (html (anchor:page-body-html post)))
    (true (search "src=\"/_media/portrait.svg\"" html)
          "The rendered page contains the resolved path because the node was
rewritten before cl-adoc rendered it.")
    (false (search "src=\"_media/portrait.svg\"" html)
           "Nothing rewrites HTML afterward. The tree was changed.")))

(define-test (reference missing)
  (let* ((root (fixture-site))
         (site (anchor:make-site root)))
    (is equal '() (anchor::missing-references site)
        "The sample site has no broken references.")))

(define-test (reference url-merging)
  (is string= "/posts/2026/first-post/"
      (anchor::merge-url "/posts/2026/second-post/" "../first-post/"))
  (is string= "/posts/" (anchor::merge-url "/posts/2026/" "../"))
  (is string= "/a/b/c/" (anchor::merge-url "/a/b/" "./c/")))

(define-test (reference scheme-detection)
  (true (anchor::url-scheme-p "https://example.org/"))
  (true (anchor::url-scheme-p "mailto:someone@example.org"))
  (true (anchor::url-scheme-p "//example.org/x")
        "A protocol-relative URL is outside the site.")
  (false (anchor::url-scheme-p "diagram.svg"))
  (false (anchor::url-scheme-p "../first-post/")))
