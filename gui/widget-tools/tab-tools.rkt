#lang racket

(provide draw-tab)

(define (draw-tab tab-panel panel-index panel-controls)
  ;; Tells tab-panel to show the given panel with the given controls.
  (send tab-panel set-selection panel-index)
  (send tab-panel change-children (lambda (c) panel-controls)))
