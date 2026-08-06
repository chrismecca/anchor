;;;; Integrity checks.

(in-package #:anchor/tests)

(define-test check)

(defun checked ()
  (anchor:check (fixture-site)))

(defun relative-names (site paths)
  (sort (mapcar (lambda (path)
                  (namestring (uiop:enough-pathname path (anchor:site-root site))))
                paths)
        #'string<))

(define-test (check passes-on-a-sound-site)
  (let ((report (checked)))
    (is equal '() (anchor:report-missing report))
    (true (anchor:report-passed-p report))))

(define-test (check finds-missing)
  (let* ((root (broken-fixture-site))
         (report (anchor:check root)))
    (is = 1 (length (anchor:report-missing report)))
    (false (anchor:report-passed-p report)
           "A missing reference is the only thing that makes a site wrong.")
    (is string= "nonexistent.png"
        (anchor::reference-target (first (anchor:report-missing report))))))

(define-test (check orphans)
  (let* ((report (checked))
         (names (relative-names (anchor:report-site report)
                                (anchor:report-orphans report))))
    (true (member "_media/unused.svg" names :test #'string=)
          "A file nothing points at is reported, which is the whole reason
_media/ is walked here even though discovery skips it.")
    (true (member "_media/shared.txt" names :test #'string=))
    (false (member "_media/portrait.svg" names :test #'string=)
           "What a page does point at is not an orphan.")
    (false (member "posts/2026/first-post/diagram.svg" names :test #'string=))
    (true (member "style.css" names :test #'string=)
          "A stylesheet is named by a layout rather than by a document, so it
lands here too. That is why orphans are advice and never a deletion.")))

(define-test (check duplicates)
  (let* ((report (checked))
         (groups (anchor:report-duplicates report))
         (names (mapcar (lambda (group)
                          (relative-names (anchor:report-site report) group))
                        groups)))
    (is = 1 (length groups))
    (is equal '("about/diagram.svg" "posts/2026/first-post/diagram.svg")
        (first names)
        "Identical content under different names, found by hash and then
confirmed byte for byte.")))

(define-test (check content-identity)
  (let ((a (merge-pathnames "about/diagram.svg" (fixture-site)))
        (b (merge-pathnames "posts/2026/first-post/diagram.svg" (fixture-site)))
        (c (merge-pathnames "_media/portrait.svg" (fixture-site))))
    (is = (anchor::content-hash a) (anchor::content-hash b))
    (isnt = (anchor::content-hash a) (anchor::content-hash c))))

(define-test (check weight)
  (let* ((report (checked))
         (weights (anchor:report-weights report))
         (post (find "/posts/2026/first-post/" weights
                     :key (lambda (w) (anchor:page-url (anchor:page-weight-page w)))
                     :test #'string=)))
    (true (plusp (anchor:page-weight-html post)))
    (is = (anchor::file-size (merge-pathnames "posts/2026/first-post/diagram.svg"
                                              (fixture-site)))
        (anchor:page-weight-assets post)
        "A page weighs its own HTML plus the media it points at.")
    (is = (+ (anchor:page-weight-html post) (anchor:page-weight-assets post))
        (anchor:page-weight-total post))
    (true (>= (anchor:page-weight-total (first weights))
              (anchor:page-weight-total (car (last weights))))
          "Heaviest first, which is the order that is useful.")))

(define-test (check report-prints)
  (let ((text (with-output-to-string (out) (anchor:print-report (checked) out))))
    (true (search "missing" text))
    (true (search "orphans" text))
    (true (search "duplicates" text))
    (true (search "weight" text))
    (true (search "resolutions" text))
    (true (search "\"_media/portrait.svg\" -> /_media/portrait.svg" text)
          "Every reference is reported as written and as resolved. That is
what the _media/ walk owes for being a search.")))
