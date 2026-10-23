;;;; Development server tests.

(in-package #:anchor/tests)

(define-test serve)

(defun http-get (port target)
  "Make one request.

Return three values: status line, headers, and body."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t
                          :element-type '(unsigned-byte 8))))
             (write-sequence
              (sb-ext:string-to-octets
               (format nil "GET ~a HTTP/1.1~c~cHost: localhost~c~c~c~c"
                       target #\Return #\Newline #\Return #\Newline
                       #\Return #\Newline)
               :external-format :utf-8)
              stream)
             (finish-output stream)
             (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                        :adjustable t :fill-pointer t)))
               (loop for byte = (read-byte stream nil nil)
                     while byte do (vector-push-extend byte bytes))
               (let* ((text (sb-ext:octets-to-string
                             (coerce bytes '(vector (unsigned-byte 8)))
                             :external-format :latin-1))
                      (split (search (format nil "~c~c~c~c"
                                             #\Return #\Newline
                                             #\Return #\Newline)
                                     text))
                      (head (subseq text 0 split))
                      (body (subseq text (+ split 4)))
                      (newline (position #\Newline head)))
                 (values (string-right-trim '(#\Return) (subseq head 0 newline))
                         head
                         body)))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defmacro with-server ((server) &body body)
  "Serve a throwaway copy of the sample site on a system-chosen port."
  `(let* ((source (temporary-output))
          (,server nil))
     (unwind-protect
          (progn
            (uiop:collect-sub*directories (fixture-site) t t (constantly t))
            ;; Serve a copy. The server builds into the tree, and the fixture
            ;; is checked in.
            (copy-tree-to (fixture-site) source)
            (setf ,server (anchor:serve source :port 0))
            ,@body)
       (when ,server (anchor:stop ,server))
       (uiop:delete-directory-tree source :validate t
                                          :if-does-not-exist :ignore))))

(defun copy-tree-to (from to)
  (ensure-directories-exist to)
  (dolist (file (uiop:directory-files from))
    (uiop:copy-file file (merge-pathnames (file-namestring file) to)))
  (dolist (directory (uiop:subdirectories from))
    (copy-tree-to directory
                  (merge-pathnames
                   (concatenate 'string
                                (car (last (pathname-directory directory)))
                                "/")
                   to))))

(define-test (serve pages)
  (with-server (server)
    (multiple-value-bind (status head body) (http-get (anchor:server-port server) "/")
      (true (search "200" status))
      (true (search "text/html" head))
      (true (search "<h1>A Sample Site</h1>" body)))
    (multiple-value-bind (status head body)
        (http-get (anchor:server-port server) "/posts/2026/first-post/")
      (declare (ignore head))
      (true (search "200" status))
      (true (search "The First Post" body)
            "A directory is served as the index.html inside it."))))

(define-test (serve serves-assets)
  (with-server (server)
    (multiple-value-bind (status head) (http-get (anchor:server-port server) "/style.css")
      (true (search "200" status))
      (true (search "text/css" head) "Content type comes from the extension."))
    (multiple-value-bind (status head)
        (http-get (anchor:server-port server) "/_media/portrait.svg")
      (true (search "200" status))
      (true (search "image/svg+xml" head)))))

(define-test (serve not-found)
  (with-server (server)
    (multiple-value-bind (status) (http-get (anchor:server-port server) "/nope.html")
      (true (search "404" status)))))

(define-test (serve refuses-to-climb-out)
  (with-server (server)
    (multiple-value-bind (status)
        (http-get (anchor:server-port server) "/../../../etc/passwd")
      (true (search "404" status)
            "A target that escapes the tree is not a valid request, even on
loopback."))))

(define-test (serve no-caching)
  (with-server (server)
    (multiple-value-bind (status head) (http-get (anchor:server-port server) "/")
      (declare (ignore status))
      (true (search "no-store" head)
            "A cached response would hide the rebuild that just ran."))))

(define-test (serve includes-drafts)
  (with-server (server)
    (multiple-value-bind (status) (http-get (anchor:server-port server)
                                            "/posts/2026/a-draft/")
      (true (search "200" status)
            "serve includes drafts. That lets a draft be previewed at the URL
it will keep."))))

(define-test (serve holds-the-graph)
  (with-server (server)
    (true (anchor:server-port server))
    (true (anchor:page "posts/2026/first-post")
          "The graph is in the image while the server runs. PAGE accepts the
slashes a person is likely to type at a prompt.")
    (is string= "The First Post" (anchor:page-title
                                  (anchor:page "/posts/2026/first-post/")))
    (false (anchor:page "posts/nothing-here"))))

(define-test (serve rebuilds-on-change)
  (with-server (server)
    (let ((source (anchor:server-root server)))
      (multiple-value-bind (status head body)
          (http-get (anchor:server-port server) "/about/")
        (declare (ignore status head))
        (true (search "<h1>About</h1>" body)))
      ;; Edit the source and request the page again. Nothing watches files.
      ;; The next request notices that the file is newer than the build.
      (sleep 1.1)
      (with-open-file (out (merge-pathnames "about/index.adoc" source)
                           :direction :output :if-exists :supersede)
        (format out "= About, Rewritten~%~%New body.~%"))
      (multiple-value-bind (status head body)
          (http-get (anchor:server-port server) "/about/")
        (declare (ignore status head))
        (true (search "<h1>About, Rewritten</h1>" body)
              "The next request rebuilt the site and served the edit.")))))

(define-test (serve reports-failed-rebuilds)
  (with-server (server)
    (let ((config (merge-pathnames "about/_anchor.lisp"
                                   (anchor:server-root server))))
      (sleep 1.1)
      (with-open-file (out config :direction :output :if-exists :supersede)
        (format out "(error \"a broken configuration\")~%"))
      (multiple-value-bind (status head body)
          (http-get (anchor:server-port server) "/")
        (declare (ignore head))
        (true (search "500" status)
              "A rebuild that fails answers the request. Before, the connection
closed with no response.")
        (true (search "a broken configuration" body)
              "The response says why."))
      (sleep 1.1)
      (with-open-file (out config :direction :output :if-exists :supersede)
        (format out "(list)~%"))
      (multiple-value-bind (status) (http-get (anchor:server-port server) "/")
        (true (search "200" status)
              "The next edit rebuilds, and the site is served again.")))))

(define-test (serve skips-unreadable-pages)
  (with-server (server)
    (sleep 1.1)
    (with-open-file (out (merge-pathnames "about/index.adoc"
                                          (anchor:server-root server))
                         :direction :output :if-exists :supersede)
      (format out "= About~%~%----~%never closed~%"))
    (multiple-value-bind (status) (http-get (anchor:server-port server) "/")
      (true (search "200" status)
            "serve builds with :permissive. One unreadable page is skipped, and
the rest of the site is still served."))))
