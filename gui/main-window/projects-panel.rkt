#lang racket

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

(provide change-to-projects-panel)

(require racket/gui/base)
(require "../../boinc-commands.rkt")
(require "../../boinc-structs.rkt")
(require "../widget-tools/list-tools.rkt"
         "../widget-tools/button-tools.rkt"
         "../widget-tools/tab-tools.rkt")

(define (get-projects-field projects the-field)
    ;; Get the given field from each of a list of project structs.
  (map (lambda (x) (the-field x)) projects))

(define (change-to-projects-panel tab-panel)
  ;; Draw the projects panel and its child controls
  (define projects (get-project-status))
  
  (define gpf ((curry get-projects-field) projects))

  (define project-names (gpf project-project-name))
  (define project-accounts (gpf project-userid))
  (define project-teams (gpf project-teamid))
  (define work-done (gpf project-user-total-credit))
  (define avg-work-done (gpf project-user-expavg-credit))
  (define resource-share (gpf project-resource-share))
  
  (define (add-to-list label contents)
    (add-list-column projects-list label 300 contents))
  
  (define projects-panel (new panel% [parent tab-panel]))
  (define hpane (new horizontal-pane%
                     [parent projects-panel]))
  
  (define button-panel (new vertical-panel%
                            [parent hpane]
                            [alignment (list 'center 'top)]))
  
  (define button-maker (get-simple-button-maker button-panel))
  
  (define (do-nothing) #f)
  (define update-button (button-maker "Update" do-nothing))
  (define suspend-button (button-maker "Suspend" do-nothing))
  (define no-new-tasks-button (button-maker "No new tasks" do-nothing))
  (define detach-project (button-maker "Detach" do-nothing))
  
  (define projects-list (new-list-box hpane 1000 project-names))
  (send projects-list set-column-width 0 300 0 1000000)
  (send projects-list set-column-label 0 "Project")
  
  (add-to-list "Account" project-accounts)
  (add-to-list "Team" project-teams)
  (add-to-list "Work done" work-done)
  (add-to-list "Avg. work done" avg-work-done)
  (add-to-list "Resource share" resource-share)    
  
  (draw-tab tab-panel 1 (list projects-panel)))
