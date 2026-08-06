;;;; The development server.
;;;;
;;;; This exists so that the graph stays in the image. The listener runs in
;;;; its own thread and the REPL that started it stays usable, so a layout can
;;;; be recompiled and the next request picks it up, (page "posts/2026/x")
;;;; answers from the live graph, and re-rendering one page is a function
;;;; call. That is not a hot-reload feature; it is what having an image gets
;;;; you, and the spec records it so that it does not get designed away.
;;;;
;;;; It speaks enough HTTP to serve a static directory to a browser on
;;;; localhost, and no more. What it deliberately does not do:
;;;;
;;;;   - listen anywhere but 127.0.0.1
;;;;   - keep a connection alive, or handle more than one at a time
;;;;   - range requests, compression, conditional requests, HTTPS
;;;;
;;;; Those are the reasons this is a hundred lines rather than a dependency
;;;; on a web server. Serving one directory over loopback to one developer is
;;;; a smaller problem than the libraries that solve it in general, and the
;;;; cost of the general solution here is a dozen transitive dependencies for
;;;; a program whose output is files. This is a different judgement from the
;;;; HTML serialiser's -- that one was structural, layouts being data. This
;;;; one is only proportion, and it would be reasonable to decide otherwise.

(in-package #:anchor)

(defparameter *site* nil
  "The site the running server holds. Bound for the REPL to read.")

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
  "Enough types for what anchor emits and what a personal site carries.")

(defun content-type (path)
  (or (cdr (assoc (pathname-type path) *content-types* :test #'string-equal))
      "application/octet-stream"))

;;; The site in the image

(defun page (url)
  "The page at URL in the site currently held, for use at the REPL.
URL is forgiving about its slashes, so that what gets typed at a prompt and
what appears in a document both work."
  (when *site*
    (site-page *site* (format nil "/~{~a/~}" (url-segments url)))))

(defun rebuild ()
  "Build the served site again and hold the new graph. Returns the site."
  ;; Stamped before the build, so an edit made while one is running is seen
  ;; by the next request rather than missed.
  (setf *last-build* (get-universal-time))
  (setf *site* (build *server-root* :output *server-output*
                                    :drafts t
                                    :policy :permissive)))

(defun rebuild-page (url)
  "Render one page again and write it. Returns the path written."
  (let ((page (page url)))
    (when page
      (write-page page *server-output* *site*))))

(defun source-changed-p (root since)
  "Whether anything under ROOT was written after SINCE."
  (loop for directory in (all-directories root)
        thereis (loop for file in (uiop:directory-files directory)
                      thereis (> (file-write-date file) since))))

(defun ensure-fresh ()
  "Rebuild if the source has changed since the last build.
Checked per request rather than watched, which needs no dependency and no
thread, and costs a stat of a personal site's worth of files."
  (when (source-changed-p *server-root* *last-build*)
    (rebuild)))

;;; HTTP, to the extent required

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
  "Write one complete response to STREAM. BODY is a byte vector."
  (let ((header (with-output-to-string (out)
                  (format out "HTTP/1.1 ~d ~a" status (status-text status))
                  (write-crlf out)
                  (format out "Content-Type: ~a" type)
                  (write-crlf out)
                  (format out "Content-Length: ~d" (length body))
                  (write-crlf out)
                  ;; A development server's output must never be cached, or
                  ;; the rebuild it just did is invisible.
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
  "One CRLF-terminated line from STREAM, or NIL at end of input."
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
  "The file under OUTPUT that TARGET names, or NIL when it names none.
A target that climbs out of the tree names none. This server is bound to
loopback and serves a directory a person just built, but a path that escapes
its root is never the request that was meant."
  (let* ((path (subseq target 0 (or (position #\? target) (length target))))
         (segments (url-segments path)))
    (when (notany (lambda (segment) (string= segment "..")) segments)
      (let* ((last (car (last segments)))
             ;; A directory, or a name with no extension, means the index
             ;; inside it. Everything else is a file.
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
      ;; The headers are read and discarded. Nothing here varies on them.
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
Returns a server. The listener is on its own thread so that the REPL which
started it stays usable, which is the entire point: the graph is in the image
and answers while the server runs. PORT 0 takes whatever the system gives,
and the server records which."
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
                            ;; One bad connection must not take the server
                            ;; down; a browser closing early is routine.
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
  "Block until SERVER's thread ends. This is what the command line does."
  (sb-thread:join-thread (server-thread server) :default nil))
