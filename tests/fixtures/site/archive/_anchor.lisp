;;;; A layout selected by configuration.
;;;;
;;;; Forms before the last one are evaluated too. That lets a site define the
;;;; layout function that :layout names.
;;;;
;;;; This fixture uses a layout that anchor already ships.

(list :layout 'anchor:front-page)
