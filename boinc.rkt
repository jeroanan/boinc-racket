#lang racket

(require "boinc-xml.rkt")
(require "boinc-structs.rkt")
(require "xexpr-utils.rkt")
(require "parse.rkt")

(define (get-state)           
  (let* ((state-xml (xexpr-get-document-element (get-state-xml)))
         (client-state (cddr (get-node (cddr state-xml) 'client_state))))
    (parse-get-state client-state)))

(define (get-host-info)
    (parse-host-info (get-main-node get-host-info-xml 'host_info)))

(define (get-disk-usage)
  (let* ((disk-usage-node (get-main-node get-disk-usage-xml 'disk_usage_summary)))
    (parse-disk-usage disk-usage-node)))

(define (get-cc-status)
  (let* ((cc-status-node (get-main-node get-cc-status-xml 'cc_status)))         
    (parse-cc-status cc-status-node)))

(define (get-project-status)
    (parse-projects (get-main-node get-project-status-xml 'projects)))
         
(define (get-results [active-only #f])
    (parse-results (get-main-node (lambda () (get-results-xml active-only)) 'results)))

(define (get-daily-transfer-history)          
  (let* ((root-xml (xexpr-get-document-element (get-daily-transfer-history-xml)))
         (main-node (sublists-only (get-node (cddr root-xml) 'daily_xfers)))
         (gns (lambda (x) (get-nodes main-node x))))
    (parse-daily-transfer-history (gns 'dx))))

