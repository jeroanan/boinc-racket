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

(require "../boinc-commands.rkt"
         "../boinc-structs.rkt"
         "../macros.rkt")
(require "attach-project-window.rkt")
(require "widget-tools/button-tools.rkt"
         "widget-tools/list-tools.rkt"
	 "widget-tools/tab-tools.rkt")

(provide show-add-project-window)

(define (show-add-project-window parent)

  (define min-width 400)
  (define min-height 300)
  
  (define dialog (new dialog%
                     [label "Add Project"]
                     [parent parent]
                     [min-width min-width]
                     [min-height min-height]))

  (define (do-nothing) #f)

  (define tab-panel
    (new tab-panel%
         [parent dialog]
	 [choices (list "Project"
	                "Account Manager")]
	 [callback (lambda (tp e)
	             (case (send tp get-selection)
		       ((0) (change-to-projects-panel))
		       ((1) (change-to-account-manager-panel))))]))

  (define dt ((curry draw-tab) tab-panel))

  (define (change-to-projects-panel)
    (define available-projects
      (sort (get-all-projects-list)
            (lambda (x y) (string<? (available-project-name x)
                                    (available-project-name y)))))
  
    (define available-project-names (map (lambda (x) (available-project-name x))
                                         available-projects))
  
    (define projects-panel
      (new panel%
        [parent tab-panel]))
  
    (define vpanel
      (new vertical-panel%
        [parent projects-panel]))

    (define projects-list (new-list-box vpanel min-width available-project-names))
    (send projects-list min-height min-height)
  
    (define (next-button-click)
      (define selected-data (get-listbox-selected-data projects-list))
      (aif (available-project? selected-data)
          (show-attach-project-window dialog (available-project-url selected-data))))
  
    (define button-container (new horizontal-pane%
                                  [parent vpanel]
                                  [alignment (list 'right 'center)]))
  
    (define button-maker (get-simple-button-maker button-container))
    (define next-button (button-maker "&Next" next-button-click))
    (define cancel-button (button-maker "&Cancel"
                                        (lambda () (send dialog show #f))))
  
    (set-listbox-data projects-list available-projects)
  
    (send projects-list set-column-label 0 "Name")
    (send projects-list set-selection 0)
    (send projects-list focus)
    (draw-tab tab-panel 0 (list projects-panel)))
  
  (define (change-to-account-manager-panel)
    (define account-manager-panel
      (new panel%
        [parent tab-panel]))
    
    (define vpanel
      (new vertical-panel%
        [parent account-manager-panel]))
	
    (define account-manager-url
      (new text-field%
        [label "URL"]
	[parent vpanel]))

    (define username
      (new text-field%
        [label "Username"]
	[parent vpanel]))

    (define password
      (new text-field%
        [label "Password"]
	[parent vpanel]
	[style (list 'single 'password)]))

    (define button-container (new horizontal-pane%
                                [parent vpanel]
                                [alignment (list 'right 'center)]))

    (define button-maker (get-simple-button-maker button-container))
    (draw-tab tab-panel 1 (list account-manager-panel))
    
    (define cancel-button (button-maker "&Cancel" (lambda () (send dialog show #f))))
    (define ok-button (button-maker "&OK" do-nothing))
    (draw-tab tab-panel 1 (list account-manager-panel)))
    
  (change-to-projects-panel)
  (send dialog show #t))
                     
