;; Copyright (c) 2016 David Wilson (Jeroanan)

;; This file is part of boinc-racket.

;; boinc-racket is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; boinc-racket is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with boinc-racket.  If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require racket/gui/base)

(provide get-simple-button-maker)

(define (get-simple-button-maker parent [min-width null])
  ;; Get a function that can be used to create a button with a label and an
  ;; on-click event.
  ;;
  ;; Parameters:
  ;;
  ;; parent: The control that will be the parent of the new button
  ;;
  ;; min-width: Set the min-width of the new buttons if provided.
  ;;            useful if a group of uniformly-sized buttons is desired.
  ;;
  ;; Returns:
  ;; A function taking the following parameters:
  ;;
  ;; label: The label that the button should have
  ;; on-click: A function with no arguments that will be executed when the
  ;; button is clicked.
  (lambda (label on-click)
    (define new-button (new button%
                            [parent parent]
                            [label label]
                            [callback
                             (make-button-click-callback on-click)]))
    (unless (null? min-width) (send new-button min-width min-width))
    new-button))
      

(define (make-button-click-callback callback-func)
  (lambda (sender control-event)
    (define event-type (send control-event get-event-type))
    (cond
      [(eq? event-type 'button) (callback-func)]
      [else #f])))
