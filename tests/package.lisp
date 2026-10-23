;;;; package.lisp

(defpackage #:anchor/tests
  (:use #:cl #:parachute)
  (:export #:run))

(in-package #:anchor/tests)

(defun fixture-site ()
  "Return the sample site used by discovery and build tests."
  (asdf:system-relative-pathname :anchor "tests/fixtures/site/"))

(defun broken-fixture-site ()
  "Return a site with one missing reference.

This is separate from the sample site. A broken reference in the sample site
would fail every test that builds it."
  (asdf:system-relative-pathname :anchor "tests/fixtures/broken/"))

(defun undated-fixture-site ()
  "Return a site where one page in a dated section has no date."
  (asdf:system-relative-pathname :anchor "tests/fixtures/undated/"))

(defun unreadable-fixture-site ()
  "Return a site with one page cl-adoc cannot parse and one page it can."
  (asdf:system-relative-pathname :anchor "tests/fixtures/unreadable/"))
