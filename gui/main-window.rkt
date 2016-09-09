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

(provide launch-gui)

(require racket/gui/base)

(require "../boinc-commands.rkt")
(require "../boinc-structs.rkt")
(require "add-project-window.rkt")
(require "widget-tools/button-tools.rkt")

(define tab-height 300)
(define tab-width 1000)

;; The main window.
(define frame (new frame%
                   (label "BOINC-Racket")
                   (height tab-height)
                   (width tab-width)))

(define (set-list-column-items list-ctrl column-no list-items [n 0])
  ;; For the given list control and column number, set the contents.
  (define (do-add)
    (send list-ctrl set-string n (first list-items) column-no)
    (set-list-column-items list-ctrl column-no (rest list-items) (+ n 1)))
  
  (if (empty? list-items) #t (do-add)))

(define (add-list-column list-ctrl label width contents)
  ;; Append a new column to a list box with the given label, width and contents
  (define number-of-columns (length (send list-ctrl get-column-labels)))
  (send list-ctrl append-column label)
  (send list-ctrl set-column-width number-of-columns width 0 1000000)
  (set-list-column-items list-ctrl number-of-columns contents))

(define (new-list-box parent choices)
  (new list-box%
       [parent parent]
       [choices choices]
       [style (list 'single
                    'variable-columns
                    'column-headers
                    'clickable-headers)]
       [min-width tab-width]
       [stretchable-width #t]
       [stretchable-height #t]
       [label #f]))

(define (change-to-notices-panel)
  ;; Draw the notices panel
  (define notices-panel (new-panel "Notices"))
  (draw-tab 0 (list notices-panel)))

(define (change-to-projects-panel)
  ;; Draw the projects panel and its child controls
  (define projects (get-project-status))
  
  (define (gpf the-field)
    ;; Get the given field from each of a list of project structs.
    (map (lambda (x) (the-field x)) projects))
  
  (define project-names (gpf project-project-name))
  (define project-accounts (gpf project-userid))
  (define project-teams (gpf project-teamid))
  (define work-done (gpf project-user-total-credit))
  (define avg-work-done (gpf project-user-expavg-credit))
  (define resource-share (gpf project-resource-share))
  
  (define (add-to-list label contents)
    (add-list-column projects-list label 300 contents))
  
  (define projects-panel (new-panel "Attached Projects"))
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
  
  (define projects-list (new-list-box hpane project-names))
  (send projects-list set-column-width 0 300 0 1000000)
  (send projects-list set-column-label 0 "Project")
  
  (add-to-list "Account" project-accounts)
  (add-to-list "Team" project-teams)
  (add-to-list "Work done" work-done)
  (add-to-list "Avg. work done" avg-work-done)
  (add-to-list "Resource share" resource-share)    
  
  (draw-tab 1 (list projects-panel)))

(define (change-to-tasks-panel)
  
  (define results (get-results))
  
  (define (gtf the-field)
    (map (lambda (x) (the-field x)) results))
  
  (define task-names (gtf result-name))
  (define project-urls (gtf result-project-url))
  (define report-deadlines (gtf result-report-deadline))
  (define task-states (gtf result-state))
  
  (define (get-fraction-done)
    (map (lambda (x) (active-task-fraction-done (result-active-tasks x))) results))
  
  (define tasks-panel (new-panel "Tasks"))
  
  (define tasks-list (new-list-box tasks-panel project-urls))
  (send tasks-list set-column-width 0 300 0 1000000)
  (send tasks-list set-column-label 0 "Project")

  (define (add-to-list label contents)
    (add-list-column tasks-list label 300 contents))

  (add-to-list "% Done" (get-fraction-done))
  (add-to-list "Deadline" report-deadlines)
  (add-to-list "Status" task-states)    
  (add-to-list "Name" task-names)

  (draw-tab 2 (list tasks-panel)))

(define (change-to-transfers-panel)
  (define transfers-panel (new-panel "Transfers"))
  (draw-tab 3 (list transfers-panel)))

(define (change-to-statistics-panel)
  (define statistics-panel (new-panel "Statistics"))
  (draw-tab 4 (list statistics-panel)))

(define (change-to-disk-panel)
  (define disk-panel (new-panel "Disk"))
  (draw-tab 5 (list disk-panel)))  

(define (get-tab-panel)
  ;; Initialise the tab strip that runs along the top of the main window.
  (new tab-panel%
       [parent frame]
       [choices (list "Notices"
                      "Projects"
                      "Tasks"
                      "Transfers"
                      "Statistics"
                      "Disk")]
       [callback (lambda (tp e)
                   (case (send tp get-selection)
                     ((0) (change-to-notices-panel))
                     ((1) (change-to-projects-panel))
                     ((2) (change-to-tasks-panel))
                     ((3) (change-to-transfers-panel))
                     ((4) (change-to-statistics-panel))
                     ((5) (change-to-disk-panel))))]))

(define tab-panel (get-tab-panel))

(define (new-panel label)  
  (new panel% [parent tab-panel]))

(define (draw-tab panel-index panel-controls)
  ;; Tells tab-panel to show the given panel with the given controls.
  (send tab-panel set-selection panel-index)
  (send tab-panel change-children (lambda (c) panel-controls)))

(define (launch-gui)

  (define menu-bar (new menu-bar%
                        (parent frame)))

  (define (new-menu label)
    (new menu% (parent menu-bar) (label label)))
  
  (define (new-menu-item label parent callback)
    (new menu-item% [label label] [parent parent] [callback callback]))

  (define (dud-menu-item label parent)
    (new-menu-item label parent (lambda (x y) #f)))

  (define file-menu (new-menu "&File"))
  (new-menu-item "&Close Window" file-menu (lambda (x y) (send frame show #f)))
  (dud-menu-item "Exit" file-menu)
  
  (define (make-dud-menu parent-menu items)
    (define (add-menu-item label)
      (dud-menu-item label parent-menu)
      (make-dud-menu parent-menu (rest items)))
    (if (empty? items)
        #f
        (add-menu-item (first items))))

  (define view-menu (new-menu "&View"))

  (define (view-menu-item caption f)
    (new-menu-item caption view-menu (lambda (x y) (f))))

  (view-menu-item "&Notices" change-to-notices-panel)
  (view-menu-item "&Projects" change-to-projects-panel)
  (view-menu-item "&Tasks" change-to-tasks-panel)
  (view-menu-item "Trans&fers" change-to-transfers-panel)
  (view-menu-item "&Statistics" change-to-statistics-panel)
  (view-menu-item "&Disk usage" change-to-disk-panel)
  
  (define tools-menu (new-menu "&Tools"))
  (new-menu-item "&Add project or account manager..." tools-menu (lambda (x y) (show-add-project-window frame)))

  (define tools-menu-items (list "&Add project or account manager..."
                                 "&Options..."
                                 "Computing &preferences..."))
  (make-dud-menu tools-menu tools-menu-items)

  ;;(define activity-menu (new-menu "&Activity"))
  (define advanced-menu (new-menu "A&dvanced"))
  (define advanced-menu-items (list "Select computer..."
                                   "Shut down connect client..."
                                   "Run CPU benchmarks"
                                   "Do network communication"
                                   "Read config files"
                                   "Read local prefs file"
                                   "Event log..."
                                   "Event log diagnostic flags..."))
  (make-dud-menu advanced-menu advanced-menu-items)

  (send frame show #t)
  (send tab-panel set-selection 1)
  (change-to-tasks-panel))
