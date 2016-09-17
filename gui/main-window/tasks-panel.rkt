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
         "../widget-tools/tab-tools.rkt"
         "../widget-tools/button-tools.rkt")

(define (change-to-tasks-panel tab-panel)
  
  (define results (get-results))
  
  (define (gtf the-field) (map the-field results))
  
  (define task-names (gtf result-name))
  (define project-urls (gtf result-project-url))
  (define report-deadlines (gtf result-report-deadline))
  (define task-states (gtf result-state))
  
  (define (get-fraction-done)
    (map (lambda (x) (active-task-fraction-done (result-active-tasks x))) results))
  
  (define tasks-panel (new panel% [parent tab-panel]))
  (define hpane (new horizontal-pane% [parent tasks-panel]))

  (define button-panel (new vertical-panel%
                            [parent hpane]
                            [alignment (list 'center 'top)]))                           
 
  (define (toggle-all-buttons-enabled enabled?)
    (for-each (lambda (x) (send x enable enabled?)) buttons))

  (define (disable-all-buttons) (toggle-all-buttons-enabled #f))
  (define (enable-all-buttons) (toggle-all-buttons-enabled #t))
  
  (define (suspend-button-change selected-data)
    (define enable-suspend-button? 
      (if (result-suspended-via-gui? selected-data) #f #t))
    (send suspend-button enable enable-suspend-button?)
    (send resume-button enable (not enable-suspend-button?)))

  (define (tasks-list-callback)
    (define selected-item (send tasks-list get-selection))
    (define selected-data (if (eq? selected-item #f)
                              #f
                              (send tasks-list get-data selected-item)))
    (when (eq? selected-data #f) (disable-all-buttons))
    (when (result? selected-data) (begin
                                  (enable-all-buttons)
                                  (suspend-button-change selected-data)
                                  selected-data
                                  selected-data)))

  (define tasks-list (new-list-box hpane 1000 project-urls tasks-list-callback))
  (send tasks-list set-column-width 0 300 0 1000000)
  (send tasks-list set-column-label 0 "Project")
  (set-listbox-data tasks-list results)

  (define button-maker (get-simple-button-maker button-panel 141))

  (define (do-nothing) #f)

  (define-syntax (op-click stx-obj)
    (syntax-case stx-obj (op-click)
      [(_ operation)
       #`(lambda ()
           (define selected-data
             (get-listbox-selected-data tasks-list))
           (when (result? selected-data) (operation [result-name selected-data]                                                    
                                                    [result-project-url
                                                     selected-data])))]))  
  
  (define suspend-button (button-maker "Suspend" (op-click suspend-result)))
  (define resume-button (button-maker "Resume" (op-click resume-result)))
  (define abort-button (button-maker "Abort" (op-click abort-result)))
  (define properties-button (button-maker "Properties" do-nothing))

  (define buttons (list suspend-button
                        resume-button
                        abort-button
                        properties-button))
	
  (define (add-to-list label contents)
    (add-list-column tasks-list label 300 contents))

  (add-to-list "% Done" (get-fraction-done))
  (add-to-list "Deadline" report-deadlines)
  (add-to-list "Status" task-states)    
  (add-to-list "Name" task-names)

  (draw-tab tab-panel 2 (list tasks-panel)))
  
