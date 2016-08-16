#lang racket

(require file/md5)
(require "boinc-xml.rkt")
(require "boinc-structs.rkt")
(require "xexpr-utils.rkt")
(require "parse.rkt")
(require "socket-util.rkt")

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

(define (get-messages)
  (let ((nodes (get-main-node get-messages-xml 'msgs)))
    (parse-messages nodes)))

(define (get-message-count)
  (let* ((root-xml (xexpr-get-document-element (get-message-count-xml)))
         (main-node (get-node root-xml 'seqno)))
    (third main-node)))

(define (get-notices-public)
  ;; To be done -- currently I have empty notices
  (get-notices-public-xml))

(define (get-screensaver-tasks)
  (let* ((root-xml (xexpr-get-document-element (get-screensaver-tasks-xml)))
         (main-node (sublists-only (get-node root-xml 'handle_get_screensaver_tasks))))
  (parse-screensaver-tasks main-node)))

(define (get-simple-gui-info)
  (let ((main-node (get-main-node get-simple-gui-info-xml 'simple_gui_info)))
    (parse-simple-gui-info main-node)))

(define (get-statistics)
  (let ((main-node (get-main-node get-statistics-xml 'statistics)))
    (parse-statistics main-node)))

(define (get-cc-config)
  (simple-authorized-action get-cc-config-xml))

(define (get-notices)
  (simple-authorized-action get-notices-xml))

(define (get-account-manager-info)
  (simple-authorized-action get-account-manager-info-xml))

(define (get-global-prefs-file)
  (simple-authorized-action get-global-prefs-file-xml))

(define (get-project-init-status)
  ;; is some other parameter needed for this?
  (simple-authorized-action get-project-init-status-xml))

(define (simple-authorized-action action)
  (define-values (cin cout) (maybe-get-socket null null))
  (authorize cin cout)
  (let ((result (action cin cout)))
    (maybe-close-socket null cin cout)
    result))

(define (get-socket)
  (tcp-connect "localhost" 31416))

(define (authorize [sock-in null] [sock-out null])

  (define-values (cin cout) (maybe-get-socket sock-in sock-out))

  (define (get-nonce)
    (let* ((root-xml (xexpr-get-document-element (auth1-xml cin cout)))
           (main-node (get-node root-xml 'nonce))
           (nonce (third main-node)))
      nonce))
  
  (define (get-password)
    (file->string "/tmp/gui_rpc_auth.cfg"))

  (let* ((string-to-encode (string-append (get-nonce) (get-password)))
         (nonce-hash (bytes->string/utf-8 (md5 string-to-encode))))         
    (auth2-xml nonce-hash cin cout)))
