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

  (define (make-button label on-click)
    (define button-width 141)
    (define new-button (button-maker label on-click))
    (send new-button min-width button-width)
    new-button)

  (define (do-nothing) #f)

  (define-syntax (op-click stx-obj)
    (syntax-case stx-obj (op-click)
      [(_ operation)
       #`(lambda ()
           (define selected-data
             (get-listbox-selected-data projects-list))
           (aif (project? selected-data) (operation (project-master-url
                                                     selected-data))))]))

  (define update-button (make-button "Update" (op-click
                                                      project-update)))  
  
  (define suspend-button (make-button "Suspend" (op-click
                                                       project-suspend)))

  (define resume-button (make-button "Resume" (op-click project-resume)))
  
  (define no-new-tasks-button (make-button "No new tasks" (op-click
                                                           project-no-more-work)))
  
  (define allow-new-tasks-button (make-button "Allow new tasks" (op-click
                                                                 project-allow-more-work)))
      
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
    
  (define detach-project-button (make-button "Detach" detach-project-click))

  (define buttons (list update-button
                        suspend-button
                        resume-button
                        no-new-tasks-button
                        allow-new-tasks-button
                        detach-project-button))                        

  (define (disable-all-buttons)
    (for-each (lambda (x) (send x enable #f)) buttons))

  (define (enable-all-buttons)
    (for-each (lambda (x) (send x enable #t)) buttons))

  (define (project-suspended-button-change selected-data)
    (define enable-suspend-button
      (if (project-suspended-via-gui? selected-data) #f #t))
    (send suspend-button enable enable-suspend-button)
    (send resume-button enable (not enable-suspend-button)))        

  (define (no-new-tasks-button-change selected-data)
    (define enable-no-new-tasks-button
      (if (project-dont-request-more-work? selected-data) #f #t))
    (send no-new-tasks-button enable enable-no-new-tasks-button)
    (send allow-new-tasks-button enable (not enable-no-new-tasks-button)))
  
  (define (projects-list-callback)
    (define selected-item (send projects-list get-selection))
    (define selected-data (if (eq? selected-item #f)
                              #f
                              (send projects-list get-data selected-item)))

    (when (eq? selected-data #f) (disable-all-buttons))
    (when (project? selected-data) (begin
                                     (enable-all-buttons)
                                     (project-suspended-button-change
                                      selected-data)
                                     (no-new-tasks-button-change
                                      selected-data))))
    
  (define projects-list (new-list-box hpane 1000 project-names projects-list-callback))
  (send projects-list set-column-width 0 300 0 1000000)
  (send projects-list set-column-label 0 "Project")
  (set-listbox-data projects-list projects)
  
  (add-to-list "Account" project-accounts)
  (add-to-list "Team" project-teams)
  (add-to-list "Work done" work-done)
  (add-to-list "Avg. work done" avg-work-done)
  (add-to-list "Resource share" resource-share)    
  
  (draw-tab tab-panel 1 (list projects-panel)))
