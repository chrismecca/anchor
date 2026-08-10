;;;; anchor.asd

(defsystem "anchor"
  :description "A static site generator for AsciiDoc, built on cl-adoc."
  :author "Chris Mecca"
  :license "MIT"
  :version "1.0.2"
  :homepage "https://github.com/chrismecca/anchor"
  ;; sb-bsd-sockets is an SBCL contrib. It is not fetched as a dependency.
  ;;
  ;; Only the development server uses it.
  :depends-on ("cl-adoc" (:require "sb-bsd-sockets"))
  :build-operation "program-op"
  :build-pathname "anchor"
  :entry-point "anchor:main"
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
                             (:file "check")
                             (:file "serve")
                             (:file "cli"))))
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
                             (:file "head")
                             (:file "emit")
                             (:file "reference")
                             (:file "build")
                             (:file "check")
                             (:file "xml")
                             (:file "cli")
                             (:file "serve")
                             (:file "selfhost"))))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :anchor/tests)))
