;;;; Configuration for the whole sample site.
;;;;
;;;; Every form is evaluated. The value of the last form is the configuration
;;;; plist.
;;;;
;;;; This fixture uses LIST instead of a literal plist to show that evaluation
;;;; happens.

(list :title    "A Sample Site"
      :author   "A Person"
      :base-url "https://example.com")
