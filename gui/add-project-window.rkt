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

(require "../boinc-commands.rkt")
(require "../boinc-structs.rkt")

(provide show-add-project-window)

(define (show-add-project-window parent)

  (define min-width 400)
  (define min-height 300)
  
  (define dialog (new dialog%
                     [label "Add Project"]
                     [parent parent]
                     [min-width min-width]
                     [min-height min-height]))

  (define available-projects (get-all-projects-list))
  (define available-project-names (map (lambda (x) (available-project-name x)) available-projects))

  (define projects-list (new list-box%
                             [parent dialog]
                             [choices available-project-names]
                             [style (list 'single
                                          'variable-columns
                                          'column-headers
                                          'clickable-headers)]
                             [stretchable-width #t]
                             [stretchable-height #t]
                             [min-width min-width]
                             [min-height min-height]
                             [label #f]))  

  (define (next-button-callback sender control-event)
    (define event-type (send control-event get-event-type))
    (cond
      [(eq? event-type 'button) (next-button-click)]
      [else #f]))

  (define (next-button-click)
    (define selected-item (first (send projects-list get-selections)))
    (define selected-data (send projects-list get-data selected-item))
    #f) ;; Todo: Display attach project dialog passing in URL

  (define next-button (new button%
                           [parent dialog]
                           [label "Next"]
                           [callback next-button-callback]))
  
  (define (add-data p-list [counter 0])
    (define (do-add)
      (send projects-list set-data counter (first p-list))
      (add-data (rest p-list) (+ counter 1)))

    (if (empty? p-list)
        #f
        (do-add)))

  (add-data available-projects)
        

  (send projects-list set-column-label 0 "Name")
  (send dialog show #t))
                     

