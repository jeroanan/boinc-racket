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
(require "../../boinc-commands.rkt"
         "../../boinc-structs.rkt"
         "../../macros.rkt")
(require "../widget-tools/list-tools.rkt"
         "../widget-tools/button-tools.rkt"
         "../widget-tools/tab-tools.rkt"
         "../widget-tools/caution-box.rkt")

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

  (define (make-button-width label on-click)
    (define button-width 123)
    (define new-button (button-maker label on-click))
    (send new-button min-width button-width))

  (define (do-nothing) #f)

  (define-syntax (op-click stx-obj)
    (syntax-case stx-obj (op-click)
      [(_ operation)
       #`(lambda ()
           (define selected-data
             (get-listbox-selected-data projects-list))
           (aif (project? selected-data) (operation (project-master-url
                                                     selected-data))))]))

  (define update-button (make-button-width "Update" (op-click
                                                      project-update)))  
  
  (define suspend-button (make-button-width "Suspend" (op-click
                                                       project-suspend)))
  
  (define (no-new-tasks-click)
    (display (send no-new-tasks-button get-width))
    (display "\n"))

  (define no-new-tasks-button (make-button-width "No new tasks" no-new-tasks-click))

  (define unauthorized-message (make-caution-box
                                (send tab-panel get-parent)
                                "Authorization with BOINC failed. Check your GUI RPC password settings."
                                "Authorization failed"))

  (define (show-caution-message message)
    (send (make-caution-box (send tab-panel get-parent) message "Error")
          show
          #t))
  
  (define (detach-project-click)
    (define selected-data (get-listbox-selected-data projects-list))
    (aif (project? selected-data)
        (begin
          (with-handlers
            ([(lambda (v) (equal? v 'unauthorized))
              (lambda (v) (send unauthorized-message show #t))])
            (define result (project-detach (project-master-url selected-data)))
            (cond
              [(void? result) (void)]
              [(error-message? result) (show-caution-message
                                      (error-message-message result))])))))
    
  (define detach-project-button (make-button-width "Detach" detach-project-click))
  
  (define projects-list (new-list-box hpane 1000 project-names))
  (send projects-list set-column-width 0 300 0 1000000)
  (send projects-list set-column-label 0 "Project")
  (set-listbox-data projects-list projects)
  
  (add-to-list "Account" project-accounts)
  (add-to-list "Team" project-teams)
  (add-to-list "Work done" work-done)
  (add-to-list "Avg. work done" avg-work-done)
  (add-to-list "Resource share" resource-share)    
  
  (draw-tab tab-panel 1 (list projects-panel)))
