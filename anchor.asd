;;;; anchor.asd

(defsystem "anchor"
  :description "A static site generator for AsciiDoc, built on cl-adoc."
  :author "Chris Mecca"
  :license "MIT"
  :version "0.0.1"
  :homepage "https://github.com/chrismecca/anchor"
  :depends-on ("cl-adoc")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "conditions")
                             (:file "html")
                             (:file "xml")
                             (:file "page")
                             (:file "config")
                             (:file "site")
                             (:file "reference")
                             (:file "graph")
                             (:file "layout")
                             (:file "emit")
                             (:file "build")
                             (:file "check"))))
  :in-order-to ((test-op (test-op "anchor/tests"))))

(defsystem "anchor/tests"
  :description "Test suite for anchor."
  :author "Chris Mecca"
  :license "MIT"
  :depends-on ("anchor" "parachute")
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "html")
                             (:file "discovery")
                             (:file "site")
                             (:file "config")
                             (:file "emit")
                             (:file "reference")
                             (:file "build")
                             (:file "check")
                             (:file "xml"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :anchor/tests)))
