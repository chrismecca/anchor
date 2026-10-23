;;;; The development server.
;;;;
;;;; This exists so the graph stays in the Lisp image.
;;;;
;;;; The listener runs in its own thread. The REPL that started it stays usable.
;;;; A layout can be recompiled, and the next request uses it.
;;;;
;;;; (page "posts/2026/x") reads from the live graph. Re-rendering one page is
;;;; a function call.
;;;;
;;;; This is not a hot-reload feature. It is what having an image gives you.
;;;; The specification records it so it does not get designed away.
;;;;
;;;; The server speaks enough HTTP to serve a static directory to a browser on
;;;; localhost. It does no more.
;;;;
;;;; It deliberately does not:
;;;;
;;;;   - listen anywhere but 127.0.0.1
;;;;   - keep connections alive
;;;;   - handle more than one connection at a time
;;;;   - support range requests
;;;;   - support compression
;;;;   - support conditional requests
;;;;   - support HTTPS
;;;;
;;;; That is why this is a small file instead of a dependency on a web server.
;;;; Serving one directory over loopback to one developer is a smaller problem
;;;; than the general libraries solve.
;;;;
;;;; The cost of a general server here would be many transitive dependencies
;;;; for a program whose output is files.
;;;;
;;;; This differs from the HTML serializer decision. The serializer decision is
;;;; structural: layouts are data. This decision is proportional. It would be
;;;; reasonable to choose differently.

(in-package #:anchor)

(defparameter *site* nil
  "The site currently held by the running server.

The REPL can read this value.")

(defparameter *server-root* nil)
(defparameter *server-output* nil)
(defparameter *last-build* 0)

(defparameter *content-types*
  '(("html" . "text/html; charset=utf-8")
    ("css"  . "text/css; charset=utf-8")
    ("js"   . "text/javascript; charset=utf-8")
    ("svg"  . "image/svg+xml")
    ("png"  . "image/png")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("webp" . "image/webp")
    ("woff2" . "font/woff2")
    ("xml"  . "application/xml")
    ("json" . "application/json")
    ("txt"  . "text/plain; charset=utf-8"))
  "Content types for what anchor emits and what a personal site commonly has.")

(defun content-type (path)
  (or (cdr (assoc (pathname-type path) *content-types* :test #'string-equal))
      "application/octet-stream"))

;;; Site state in the image

(defun page (url)
  "Return the page at URL in the currently held site.

URL is forgiving about slashes. Values typed at the REPL and values found in a
document both work."
  (when *site*
    (site-page *site* (format nil "/~{~a/~}" (url-segments url)))))

(defun rebuild ()
  "Build the served site again and hold the new graph.

Return the site."
  ;; Stamp before the build. Then an edit made during the build is seen by the
  ;; next request instead of missed.
  (setf *last-build* (get-universal-time))
  (setf *site* (build *server-root* :output *server-output*
                                    :drafts t
                                    :policy :permissive)))

(defun rebuild-page (url)
  "Render one page again and write it.

Return the path written."
  (let ((page (page url)))
    (when page
      (write-page page *server-output* *site*))))

(defun source-changed-p (root since)
  "Return true when any file below ROOT was written after SINCE.

Hidden files are ignored. Tools such as git and Finder rewrite them often, and
they are never part of the site."
  (loop for directory in (all-directories root)
        thereis (loop for file in (site-files directory)
                      thereis (> (file-write-date file) since))))

(defun ensure-fresh ()
  "Rebuild when the source changed since the last build.

This is checked per request instead of watched. It needs no dependency and no
extra thread. The cost is a stat of a personal site's files."
  (when (source-changed-p *server-root* *last-build*)
    (rebuild)))

;;; HTTP, only as much as the development server needs

(defun write-crlf (stream)
  (write-char #\Return stream)
  (write-char #\Newline stream))

(defun status-text (status)
  (ecase status
    (200 "OK")
    (400 "Bad Request")
    (404 "Not Found")
    (405 "Method Not Allowed")))

(defun respond (stream status type body)
  "Write one complete response to STREAM.

BODY is a byte vector."
  (let ((header (with-output-to-string (out)
                  (format out "HTTP/1.1 ~d ~a" status (status-text status))
                  (write-crlf out)
                  (format out "Content-Type: ~a" type)
                  (write-crlf out)
                  (format out "Content-Length: ~d" (length body))
                  (write-crlf out)
                  ;; Never cache development-server output. Otherwise the
                  ;; rebuild that just ran can be invisible.
                  (format out "Cache-Control: no-store")
                  (write-crlf out)
                  (format out "Connection: close")
                  (write-crlf out)
                  (write-crlf out))))
    (write-sequence (sb-ext:string-to-octets header :external-format :utf-8)
                    stream)
    (write-sequence body stream)
    (finish-output stream)))

(defun text-body (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun read-request-line (stream)
  "Return one CRLF-terminated line from STREAM.

Return NIL at end of input."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer t))
        (read-anything nil))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (setf read-anything t)
             (when (= byte 10) (return))
             (unless (= byte 13) (vector-push-extend byte bytes)))
    (when read-anything
      (sb-ext:octets-to-string (coerce bytes '(vector (unsigned-byte 8)))
                               :external-format :utf-8))))

(defun request-file (target output)
  "Return the file below OUTPUT named by TARGET.

Return NIL when TARGET names no file.

A target that climbs out of OUTPUT names no file. This server is bound to
loopback and serves a directory the user just built, but a path escape is
never the request that was meant."
  (let* ((path (subseq target 0 (or (position #\? target) (length target))))
         (segments (url-segments path)))
    (when (notany (lambda (segment) (string= segment "..")) segments)
      (let* ((last (car (last segments)))
             ;; A directory, or a name with no extension, names index.html
             ;; inside it. Everything else names a file.
             (segments (if (or (null last) (not (find #\. last)))
                           (append segments '("index.html"))
                           segments))
             (file (merge-pathnames (format nil "~{~a~^/~}" segments) output)))
        (and (probe-file file)
             (not (uiop:directory-pathname-p (probe-file file)))
             (probe-file file))))))

(defun handle-connection (stream output)
  (let ((request (read-request-line stream)))
    (when request
      ;; Read and discard headers. No behavior here depends on them.
      (loop for line = (read-request-line stream)
            while (and line (plusp (length line))))
      (let* ((parts (uiop:split-string request :separator " "))
             (method (first parts))
             (target (second parts)))
        (cond
          ((or (null method) (null target))
           (respond stream 400 "text/plain; charset=utf-8"
                    (text-body "bad request")))
          ((not (or (string= method "GET") (string= method "HEAD")))
           (respond stream 405 "text/plain; charset=utf-8"
                    (text-body "method not allowed")))
          (t
           (ensure-fresh)
           (let ((file (request-file target output)))
             (if file
                 (respond stream 200 (content-type file) (file-bytes file))
                 (respond stream 404 "text/plain; charset=utf-8"
                          (text-body (format nil "not found: ~a~%" target)))))))))))

;;; Running

(defstruct (server (:constructor %make-server))
  "A running development server."
  socket
  thread
  port
  root
  output)

(defun serve (root &key (port 4000) output)
  "Build ROOT, serve it on PORT, and rebuild when the source changes.

Return a server.

The listener runs in its own thread, so the REPL that started it stays usable.
That is the point: the graph stays in the image and can answer while the
server runs.

PORT 0 asks the system for a port. The server records the port it received."
  (setf *server-root* (resolve-root root))
  (setf *server-output* (uiop:ensure-absolute-pathname
                         (uiop:ensure-directory-pathname
                          (or output (merge-pathnames "_site/" *server-root*)))
                         (uiop:getcwd)))
  (rebuild)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    ;; Loopback only. A development server has no business on a network.
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen socket 8)
    (let* ((bound-port (nth-value 1 (sb-bsd-sockets:socket-name socket)))
           (output *server-output*)
           (server (%make-server :socket socket
                                 :port bound-port
                                 :root *server-root*
                                 :output output)))
      (setf (server-thread server)
            (sb-thread:make-thread
             (lambda ()
               (loop
                 (let ((connection (handler-case
                                       (sb-bsd-sockets:socket-accept socket)
                                     (error () (return)))))
                   (unwind-protect
                        (let ((stream (sb-bsd-sockets:socket-make-stream
                                       connection :input t :output t
                                       :element-type '(unsigned-byte 8))))
                          (handler-case (handle-connection stream output)
                            ;; One bad connection must not stop the server.
                            ;; Browsers close early during normal use.
                            (error () nil)))
                     (ignore-errors (sb-bsd-sockets:socket-close connection))))))
             :name "anchor serve"))
      server)))

(defun stop (server)
  "Stop SERVER and wait for its thread to finish."
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (ignore-errors (sb-thread:join-thread (server-thread server) :timeout 2))
  server)

(defun wait (server)
  "Block until SERVER's thread ends.

The command line uses this."
  (sb-thread:join-thread (server-thread server) :default nil))
