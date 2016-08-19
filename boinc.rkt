#lang racket

(require racket/gui/base)

(require "boinc-commands.rkt")
(require "boinc-structs.rkt")

(define (launch-gui)
  (define tab-height 300)
  (define tab-width 1000)

  (define frame (new frame%
                     (label "BOINC-Racket")
                     (height tab-height)
                     (width tab-width)))

  (define tab-panel (new tab-panel%
                         (parent frame)
                         (choices (list "Tasks"
                                        "Projects"))
                         (callback (lambda (tp e)
                                     (case (send tp get-selection)
                                       ((0) (change-to-tasks-panel))
                                       ((1) (change-to-projects-panel)))))))

  (define (new-panel label)
    (new panel%
         (parent tab-panel)))

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
  
  (define (set-list-column-items list-ctrl column-no list-items [n 0])
    (define (do-add)
      (send list-ctrl set-string n (first list-items) column-no)
      (set-list-column-items list-ctrl column-no (rest list-items) (+ n 1)))
    
    (if (empty? list-items)
        #t
        (do-add)))

  (define (add-list-column list-ctrl label width contents)
    (define number-of-columns (length (send list-ctrl get-column-labels)))
    (send list-ctrl append-column label)
    (send list-ctrl set-column-width number-of-columns width 0 1000000)
    (set-list-column-items list-ctrl number-of-columns contents))

  (define (change-to-tasks-panel)

    (define results (get-results))
    
    (define (get-task-field the-field)
      (map (lambda (x) (the-field x)) results))

    (define task-names (get-task-field result-name))
    (define project-urls (get-task-field result-project-url))
    (define report-deadlines (get-task-field result-report-deadline))
    (define task-states (get-task-field result-state))

    (define (get-fraction-done)
      (map (lambda (x) (active-task-fraction-done (result-active-tasks x))) results))

    (define tasks-panel (new-panel "Tasks"))

    (define (add-to-list label contents)
      (add-list-column tasks-list label 300 contents))
    
    (define tasks-list (new-list-box tasks-panel project-urls))
    (send tasks-list set-column-width 0 300 0 1000000)
    (send tasks-list set-column-label 0 "Project")

    (add-to-list "% Done" (get-fraction-done))
    (add-to-list "Deadline" report-deadlines)
    (add-to-list "Status" task-states)
    
    (add-to-list "Name" task-names)

    (send tab-panel change-children (lambda (c) (list tasks-panel))))

  (define (change-to-projects-panel)

    (define (get-project-list-items)
      (map (lambda (x)
             (project-project-name x)) (get-project-status)))
    
    (define projects-panel (new-panel "Attached Projects"))
    (define projects-list (new-list-box projects-panel (get-project-list-items)))
           
    (send tab-panel change-children (lambda (c) (list projects-panel))))
  
  (send frame show #t)
  (change-to-tasks-panel))
