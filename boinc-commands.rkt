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

(require file/md5)

(require "boinc-xml.rkt")
(require "boinc-structs.rkt")
(require "xexpr-utils.rkt")
(require "parse.rkt")
(require "socket-util.rkt")
(require "config.rkt")

(provide (all-defined-out))

;; By and large the functions in this module all perform the following tasks:
;; 1. Call their corresponding functions in boinc-xml.rkt to perform the correct
;;    RPC call.
;; 2. Examine the returned XML to get the "main" node. This differs from command
;;    to command but will generally be the node immediately following
;;    boinc_gui_rpc_reply.
;; 3. Call the relevant function in parse.rkt to get the XML into a struct
;;
;; Some commands require authorization. This is also taken care of in this
;; module.
;;
;; A few RPC calls remain untested. In most of these cases the functions here
;; simply return the XML without making any attempt to parse into a struct.

(define (get-state)           
  (let* ((state-xml (xexpr-get-document-element (get-state-xml)))
         (client-state (cddr (get-node (cddr state-xml) 'client_state))))
    (parse-get-state client-state)))

(define (get-host-info)
    (parse-host-info (get-main-node get-host-info-xml 'host_info)))

(define (get-disk-usage)
  (let ((disk-usage-node (get-main-node get-disk-usage-xml 'disk_usage_summary)))
    (parse-disk-usage disk-usage-node)))

(define (get-cc-status)
  (let ((cc-status-node (get-main-node get-cc-status-xml 'cc_status)))         
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
  (let* ((root-xml (xexpr-get-document-element (get-notices-public-xml)))
         (main-node (get-node root-xml 'notices)))
  (display main-node)
  (display "\n")
  (get-notices-public-xml)))

(define (get-screensaver-tasks)
  (let* ((root-xml (xexpr-get-document-element (get-screensaver-tasks-xml)))
         (main-node (sublists-only (get-node root-xml 'handle_get_screensaver_tasks))))
  (parse-screensaver-tasks main-node)))

(define (get-all-projects-list)
  (let ((main-node (get-main-node get-all-projects-list-xml 'projects)))
    (parse-available-projects main-node)))

(define (get-simple-gui-info)
  (let ((main-node (get-main-node get-simple-gui-info-xml 'simple_gui_info)))
    (parse-simple-gui-info main-node)))

(define (get-statistics)
  (let ((main-node (get-main-node get-statistics-xml 'statistics)))
    (parse-statistics main-node)))

(define (get-cc-config)
  (simple-authorized-action get-cc-config-xml))

(define (get-notices)
  (let* ((root-xml (xexpr-get-document-element (simple-authorized-action get-notices-xml)))
         (main-node (get-node root-xml 'notices)))

    (parse-notices main-node)))

(define (get-account-manager-info)
  (simple-authorized-action get-account-manager-info-xml))

(define (get-global-prefs-file)
  (simple-authorized-action get-global-prefs-file-xml))

(define (get-project-init-status)
  ;; is some other parameter needed for this?
  (simple-authorized-action get-project-init-status-xml))

(define (run-benchmarks)
  (simple-authorized-action run-benchmarks-xml))

(define (get-string-index string-in search-substring [counter 0])
  ;; Find search-substring in string-in. Returns the index of the first
  ;; character of search-subsring's first occurrence in string-in. If
  ;; search-substring is not found in string-in, return -1.
  (cond
    [(string-prefix? string-in search-substring) counter]
    ;; If it's a single character and it's not our substring it can't be there
    [(eq? (string-length string-in) 1) -1]       
    [else (get-string-index (substring string-in 1 (string-length string-in))
                            search-substring
                            (+ counter 1))]))

(define (get-inner-string string-in string-begin string-end)
  ;; Get a string from a larger string that lies between string-in and
  ;; string-end.
  ;;
  ;; This is particularly useful for getting the inner string from an XML tag.
  (define inner-start-index (+ (get-string-index string-in string-begin)
                                 (string-length string-begin)))
  (define inner-end-index (get-string-index string-in string-end))

  (if (or (eq? inner-start-index -1)
          (eq? inner-end-index -1))
      ""
      (substring string-in inner-start-index inner-end-index)))

(define (extract-error-message string-in) (get-inner-string string-in
                                                            "<error>"
                                                            "</error>"))

(define (is-lookup-pending? msg) (and
                                      (is-error? msg)
                                      (equal?
                                       (get-inner-string msg "<error_num>"
                                                         "</error_num>")
                                       "-204")))

(define (is-unauthorized? msg) (string-contains? msg "unauthorized"))
(define (is-error? msg) (string-contains? msg "error"))
(define (is-success? msg) (string-contains? msg "success"))

(define (lookup-account project-url email-address password)

  (define-values (sock-in sock-out) (maybe-get-socket null null))

  (define passsword-hash
    (bytes->string/utf-8 (md5 (string-append password email-address))))

  (define (do-lookup [sock-in null] [sock-out null])
    (lookup-account-xml project-url
                        email-address
                        passsword-hash
                        sock-in sock-out))

  (define (do-lookup-poll)
    (define (is-account-out? msg) (string-contains? msg "account_out"))
        (define (is-authenticator? msg) (string-contains? msg "authenticator"))
    
    (sleep 2) ;; This causes the main thread to lock out.
              ;; So I really need run lookup-account in a separate thread.
    (define poll-result (simple-authorized-action lookup-account-poll-xml
                                                  sock-in sock-out))

    (cond
      [(is-unauthorized? poll-result) (raise 'unauthorized)]
      [(is-lookup-pending? poll-result) (do-lookup-poll)]
      [(is-error? poll-result) (error-message (get-inner-string poll-result
                                                                "<error_msg>"
                                                                "</error_msg>"))]
      [(is-authenticator? poll-result) (project-authenticator (get-inner-string poll-result
                                                                    "<authenticator>"
                                                                    "</authenticator>"))]
      [else (error (string-append "Unknown response: " result))]))    
  
  (define result (simple-authorized-action do-lookup sock-in sock-out))
  (cond
    [(is-unauthorized? result) (raise 'unauthorized)]
    [(is-success? result) (do-lookup-poll)] 
    [(is-error? result) (error-message (extract-error-message result))]
    [else (error (string-append "Unknown response: " result))]))

(define (project-attach project-url authenticator [sock-in null] [sock-out
                                                                  null])
  (define (do-attach [sock-in null] [sock-out null])
    (project-attach-xml project-url authenticator sock-in sock-out))
  (simple-authorized-action do-attach sock-in sock-out))    

(define (project-detach project-url [sock-in null] [sock-out null])
  (define (do-detach [sock-in null] [sock-out null])
    (project-detach-xml project-url sock-in sock-out))
  (define result (simple-authorized-action do-detach sock-in sock-out))
  (cond
    [(is-unauthorized? result) (raise 'unauthorized)]
    [(is-error? result) (error-message (get-inner-string result
                                                         "<error>"
                                                         "</error>"))]
    [(is-success? result) (void)]
    [else (error (string-append "Unknown response: " result))]))

(define (project-update project-url [sock-in null] [sock-out null])
  (project-authorized-action project-update-xml project-url sock-in sock-out))

(define (project-suspend project-url [sock-in null] [sock-out null])
  (project-authorized-action project-suspend-xml project-url sock-in sock-out))

(define (project-resume project-url [sock-in null] [sock-out null])
  (project-authorized-action project-resume-xml project-url sock-in sock-out))

(define (project-no-more-work project-url [sock-in null] [sock-out null])
  (project-authorized-action project-no-more-work-xml project-url sock-in
                             sock-out))

(define (project-allow-more-work project-url [sock-in null] [sock-out null])
  (project-authorized-action project-allow-more-work-xml project-url sock-in
                             sock-out))

(define (suspend-result result-name project-url [sock-in null] [sock-out null])
  (result-authorized-action suspend-result-xml result-name project-url sock-in
                            sock-out))

(define (resume-result result-name project-url [sock-in null] [sock-out null])
  (result-authorized-action resume-result-xml result-name project-url sock-in sock-out))

(define (abort-result result-name project-url [sock-in null] [sock-out null])
  (result-authorized-action abort-result-xml result-name project-url sock-in sock-out))
  
(define (get-project-config) 
  
  (define-values (sock-in sock-out) (maybe-get-socket null null))

  (define (get-project-config-poll) 
    (define poll-result (simple-authorized-action get-project-config-poll-xml sock-in sock-out))
    (sleep 2) ;; This causes the main thread to lock out.
    (cond
      [(is-lookup-pending? poll-result) (get-project-config-poll)]))

  (define (do-operation sock-in sock-out)
    (get-project-config-xml "https://bam.boincstats.com" sock-in sock-out))

  (simple-authorized-action do-operation sock-in sock-out)
  (get-project-config-poll)) 

(define (account-manager-rpc url username password [sock-in null] [sock-out null])
  (define (poll)
    ;; error -189 == Invalid URL
    ;;       -112 == Invalid username/password
    ;;          0 == success

    (define poll-result (simple-authorized-action account-manager-rpc-poll-xml sock-in sock-out))
    (sleep 2) ;; This causes the main thread to lock out.
    (cond 
      [(is-lookup-pending? poll-result) (poll)]))

  (define (do-operation sock-in sock-out)
        (account-manager-rpc-xml url username password sock-in sock-out)
    (poll))

  (simple-authorized-action do-operation sock-in sock-out))

(define (project-authorized-action xml-op project-url [sock-in null] [sock-out null])
  (define-values (cin cout) (maybe-get-socket sock-in sock-out))
  (authorize cin cout)
  (xml-op project-url cin cout))

(define (result-authorized-action xml-op result-name project-url [sock-in null]
                                  [sock-out null])
  (define-values (cin cout) (maybe-get-socket sock-in sock-out))
  (authorize cin cout)
  (xml-op result-name project-url cin cout))
                     
(define (simple-authorized-action action [sock-in null] [sock-out null])
  (define-values (cin cout) (maybe-get-socket sock-in sock-out))
  (authorize cin cout)
  (action cin cout))

(define (authorize [sock-in null] [sock-out null])

  (define-values (cin cout) (maybe-get-socket sock-in sock-out))

  (define (get-nonce)
    (let* ((root-xml (xexpr-get-document-element (auth1-xml cin cout)))
           (main-node (get-node root-xml 'nonce))
           (nonce (third main-node)))
      nonce))
  
  (define (get-password)
    (file->string (get-gui-rpc-auth-file-location)))

  (let* ((string-to-encode (string-append (get-nonce) (get-password)))
         (nonce-hash (bytes->string/utf-8 (md5 string-to-encode))))         
    (auth2-xml nonce-hash cin cout)))
