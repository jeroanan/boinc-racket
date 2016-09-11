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

(provide change-to-tasks-panel)

(require racket/gui/base)
(require "../../boinc-commands.rkt"
         "../../boinc-structs.rkt")
(require "../widget-tools/list-tools.rkt"
         "../widget-tools/tab-tools.rkt")

(define (change-to-tasks-panel tab-panel)
  
  (define results (get-results))
  
  (define (gtf the-field)
    (map (lambda (x) (the-field x)) results))
  
  (define task-names (gtf result-name))
  (define project-urls (gtf result-project-url))
  (define report-deadlines (gtf result-report-deadline))
  (define task-states (gtf result-state))
  
  (define (get-fraction-done)
    (map (lambda (x) (active-task-fraction-done (result-active-tasks x))) results))
  
  (define tasks-panel (new panel% [parent tab-panel]))
  
  (define tasks-list (new-list-box tasks-panel 1000 project-urls))
  (send tasks-list set-column-width 0 300 0 1000000)
  (send tasks-list set-column-label 0 "Project")

  (define (add-to-list label contents)
    (add-list-column tasks-list label 300 contents))

  (add-to-list "% Done" (get-fraction-done))
  (add-to-list "Deadline" report-deadlines)
  (add-to-list "Status" task-states)    
  (add-to-list "Name" task-names)

  (draw-tab tab-panel 2 (list tasks-panel)))
  
