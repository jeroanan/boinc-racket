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

(require "../../boinc-commands.rkt"
         "../../boinc-structs.rkt"
         "../../macros.rkt")
(require "../add-project-window.rkt")
(require "projects-panel.rkt"
         "tasks-panel.rkt")
(require "../widget-tools/button-tools.rkt"
         "../widget-tools/list-tools.rkt"
         "../widget-tools/tab-tools.rkt")

(require "projects-panel.rkt")

(define tab-height 300)
(define tab-width 1000)

;; The main window.
(define frame (new frame%
                   (label "BOINC-Racket")
                   (height tab-height)
                   (width tab-width)))


(define tab-panel
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
                     ((1) ((lambda () (change-to-projects-panel tab-panel))))
                     ((2) ((lambda() (change-to-tasks-panel tab-panel))))
                     ((3) (change-to-transfers-panel))
                     ((4) (change-to-statistics-panel))
                     ((5) (change-to-disk-panel))))]))

(define dt ((curry draw-tab) tab-panel))

(define (change-to-panel index)
  ;; Draw an empty panel at the given index.
  (define new-panel (new panel% [parent tab-panel]))
  (dt index (list new-panel)))

(define (change-to-notices-panel) (change-to-panel 0))
(define (change-to-transfers-panel) (change-to-panel 3))
(define (change-to-statistics-panel) (change-to-panel 4))
(define (change-to-disk-panel) (change-to-panel 5))

(define (init-menu)
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
    (aif (not (empty? items)) (add-menu-item (first items))))
  
  (define view-menu (new-menu "&View"))
  
  (define (view-menu-item caption f)
    (new-menu-item caption view-menu (lambda (x y) (f))))
  
  (view-menu-item "&Notices" change-to-notices-panel)
  (view-menu-item "&Projects" (lambda () (change-to-projects-panel tab-panel)))
  (view-menu-item "&Tasks" (lambda () (change-to-tasks-panel tab-panel)))
  (view-menu-item "Trans&fers" change-to-transfers-panel)
  (view-menu-item "&Statistics" change-to-statistics-panel)
  (view-menu-item "&Disk usage" change-to-disk-panel)
  
  (define tools-menu (new-menu "&Tools"))
  (new-menu-item "&Add project or account manager..." tools-menu
                 (lambda (x y) (show-add-project-window frame)))
  
  (define tools-menu-items (list "&Options..."
                                 "Computing &preferences..."))
  (make-dud-menu tools-menu tools-menu-items)

  (define advanced-menu (new-menu "A&dvanced"))
  (define advanced-menu-items (list "Select computer..."
                                    "Shut down connect client..."
                                    "Run CPU benchmarks"
                                    "Do network communication"
                                    "Read config files"
                                    "Read local prefs file"
                                    "Event log..."
                                    "Event log diagnostic flags..."))
  (make-dud-menu advanced-menu advanced-menu-items))

(define (launch-gui)
  (aif (not (send frame get-menu-bar)) (init-menu))
  (send frame show #t)
  (send tab-panel set-selection 1)
  (change-to-tasks-panel tab-panel))
