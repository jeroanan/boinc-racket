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

(require racket/unix-socket)
(require "socket-util.rkt")

(provide exchange-versions-xml)
(provide get-all-projects-list-xml)
(provide get-cc-status-xml)
(provide get-cc-config-xml)
(provide get-host-info-xml)
(provide get-newer-version-xml)
(provide get-project-status-xml)
(provide get-results-xml)
(provide get-disk-usage-xml)
(provide get-daily-transfer-history-xml)
(provide get-messages-xml)
(provide get-message-count-xml)
(provide get-notices-xml)
(provide get-notices-public-xml)
(provide get-global-prefs-file-xml)
(provide get-project-init-status-xml)
(provide get-screensaver-tasks-xml)
(provide get-simple-gui-info-xml)
(provide get-state-xml)
(provide get-statistics-xml)
(provide get-account-manager-info-xml)
(provide run-benchmarks-xml)
(provide auth1-xml
         auth2-xml
         lookup-account-xml
         lookup-account-poll-xml
         project-attach-xml
         project-detach-xml
         project-update-xml
         project-suspend-xml
         project-resume-xml
         project-no-more-work-xml
         project-allow-more-work-xml
         suspend-result-xml
         resume-result-xml
         abort-result-xml
	 get-project-config-poll-xml
	 get-project-config-xml
	 account-manager-rpc-xml
	 account-manager-rpc-poll-xml
         get-file-transfers-xml)

(define (exchange-versions-xml)
  ;; makes an exchange_versions RPC call
  (rpc-call "<exchange_versions/>"))

(define (get-cc-status-xml)
  ;; Makes a cc_status RPC call
  (rpc-call "<get_cc_status/>"))

(define (get-cc-config-xml [sock-in null] [sock-out null])
  ;; Makes a get_cc_config RPC call. Requires authorization.
  (rpc-with-socket "<get_cc_config />" sock-in sock-out))

(define (get-host-info-xml)
  ;; Makes a get_host_info RPC call
  (rpc-call "<get_host_info />"))

(define (get-newer-version-xml)
  ;; Make a get_newer_version RPC call.
  (rpc-call "<get_newer_version />"))
  
(define (get-project-status-xml)
  ;; Make a get_project_status RPC call.
  (rpc-call "<get_project_status />"))

(define (get-results-xml [active-only #f])  
  ;; Make a get_results RPC call. If active-only then all results, otherwise
  ;; active ones only.
  (rpc-call
   (string-append "<get_results><active_only>"
                  (if active-only "true" "false" )
                  "</active_only></get_results>")))

(define (set-mode mode-name mode duration)
  ;; Makes rpc calls for  mode-set operations --see following functions
  (rpc-call (string-append "<set_" mode-name "_mode>"
                           "<" (string-downcase mode) "/>"
                           "<duration>" (number->string duration) "</duration>"
                           "</set_" mode-name "_mode>")))

(define (set-run-mode mode [duration 0])
  ;; Set CPU run mode
  (set-mode "run" mode duration))

(define (set-gpu-mode mode [duration 0])
  ;; Set gpu run mode
  (set-mode "gpu" mode duration))

(define (set-network-mode mode [duration 0])
  ;; Set network run mode 
  (set-mode "network" mode duration))

(define (quit)
  ;; Quit boinc client
  (rpc-call "<quit />"))

(define (result-op op result-name project-url)
  ;; Makes rpc call for a generic project operation -- see following functions
  (rpc-call
   (string-append "<" op ">"
                  "<name>" result-name "</name>"
                  "<project_url>" project-url "</project-url>"
                  "</" op ">")))

(define (abort-result-xml result-name project-url [sock-in null] [sock-out null])
  ;; Abort result
  (result-rpc-with-socket "abort_result" result-name project-url sock-in sock-out))

(define (suspend-result-xml result-name project-url [sock-in null]
                            [sock-out null])
  ;; Suspend result
  (display project-url)
  (result-rpc-with-socket "suspend_result" result-name project-url sock-in
                          sock-out))  

(define (resume-result-xml result-name project-url [sock-in null] [sock-out null])  
  ;; Resume result
  (result-rpc-with-socket "resume_result" result-name project-url sock-in sock-out))

(define (get-disk-usage-xml)
  ;; Get disk usage
  (rpc-call "<get_disk_usage />"))

(define (get-daily-transfer-history-xml)
  ;; Get daily-transfer-history
  (rpc-call "<get_daily_xfer_history />"))

(define (get-messages-xml)
  ;; Get messages
  (rpc-call "<get_messages />"))

(define (get-message-count-xml)
  ;; Get message count
  (rpc-call "<get_message_count />"))

(define (get-file-transfers-xml)
  ;; Get pending file transfers.
  (rpc-call "<get_file_transfers />"))

(define (get-notices-xml [sock-in null] [sock-out null])
  ;; Get notices. Requires authorization.
  (rpc-with-socket "<get_notices />" sock-in sock-out))

(define (get-notices-public-xml)
  ;; Get public notices. Does not require authorization.
  (rpc-call "<get_notices_public />"))

(define (get-account-manager-info-xml [sock-in null] [sock-out null])
  ;; Get account manager info. Requires authorization.
  (rpc-with-socket "<acct_mgr_info />" sock-in sock-out))

(define (get-global-prefs-file-xml [sock-in null] [sock-out null])  
  ;; Get global preferences file xml. Requires authorization.
  (rpc-with-socket "<get_global_prefs_file />" sock-in sock-out))

(define (get-project-init-status-xml [sock-in null] [sock-out null])
  ;; Get project init status xml. Requires authorization.
  (rpc-with-socket "<get_project_init_status />" sock-in sock-out))

(define (create-account url email-address password-hash username)
  ;; Create a new project account. Requires authorization.
  (rpc-call (string-append "<create_account>"
                           "<url>" url "</url>"
                           "<email_addr>" email-address "</email_addr>"
                           "<passwd_hash>" password-hash "</passwd_hash>"
                           "<ldap_auth>0</ldap_auth>"
                           "<user_name>" username "</user_name>"
                           "</create_account>")))

(define (create-account-poll)
  ;; Poll the progress of project account creation. Requires authorization.
  (rpc-call "<create_account_poll />"))

(define (lookup-account-xml project-url
                            email-address
                            password-hash
                            [sock-in null]
                            [sock-out null])
  ;; Look up an existing project account. Requires authorization.
  ;; password-hash is an MD5 hash of the account's email address concatenated to
  ;; its password.
  (rpc-with-socket
   (string-append "<lookup_account>"
                  "<url>" project-url "</url>"
                  "<email_addr>" email-address "</email_addr>"
                  "<passwd_hash>" password-hash "</passwd_hash>"
                  "<ldap_auth>0</ldap_auth>"
                  "</lookup_account>")
   sock-in
   sock-out))

(define (lookup-account-poll-xml [sock-in null] [sock-out null])
  ;; Poll the progress of project account lookup. Requires authorization.
  ;;
  ;; This function returns differenlty-shaped XML depending on the status of the
  ;; account lookup.
  ;;
  ;; Firstly, if the connection is not authorized then it gives a plain old
  ;; unauthorized response:
  ;;
  ;; <boinc_gui_rpc_reply><unauthorized /></boinc_gui_rpc_reply>
  ;;
  ;; If called while the lookup is still pending then error -204 is returned. In
  ;; that case then this function should be called again periodically until the
  ;; lookup has been completed:
  ;;
  ;; <account_out><error_num>-204</error_num></account_out>
  ;;
  ;; If an actual problem occurs, e.g. an invalid password, then the following
  ;; message will be returned:
  ;;
  ;; <error><error_num>-206</error_num><error_msg>Invalid password</error_msg></error>
  ;;  
  ;; There are other error numbers that can be returned, which may not be
  ;; covered by whatever calls this function, so the reader is encouraged to
  ;; consult lib/error_numbers.h in the BOINC source code in the event that an
  ;; unfamiliar one is seen.
  ;;
  ;; Finally, when lookup completes successfully the following kind of message
  ;; is returned:
  ;;
  ;; <account_out><authenticator>authenticator-string</authenticator></account_out>
  ;;
  ;; Where authenticator-string is a string returned that needs to be supplied
  ;; when actually attaching to the project.  
  (rpc-with-socket "<lookup_account_poll />" sock-in sock-out))

(define (get-screensaver-tasks-xml)
  ;; Get screensaver tasks
  (rpc-call "<get_screensaver_tasks />"))

(define (get-simple-gui-info-xml)
  ;; Get  simple gui info
  (rpc-call "<get_simple_gui_info />"))

(define (get-state-xml)
  ;; Get state xml
  (rpc-call "<get_state/>"))

(define (get-statistics-xml)
  ;; Get statistics
  (rpc-call "<get_statistics />"))

(define (get-all-projects-list-xml)
  ;;  Make an RPC call to get a list of all available public BOINC projects.
  (rpc-call "<get_all_projects_list />"))

(define (project-attach-xml project-url authenticator [sock-in null] [sock-out null])
  ;; Attach to a project
  ;; Parameters:
  ;;
  ;; project-url: The URL of the project to connect to
  ;; authenticator: When lookup-account is called successfully, an authetnicator
  ;;                string is given. That should be the value of this parameter.
  (rpc-with-socket
   (string-append "<project_attach>"
                  "<use_config_file>false</use_config_file>"
                  "<project_url>" project-url "</project_url>"
                  "<authenticator>" authenticator "</authenticator>"
                  "<project_name>" project-url "</project_name>"
                  "<project_attach>")
   sock-in sock-out))

(define (project-url-operation operation-name project-url)
  ;; Perform a generic operation on a project when that project requires only a
  ;; project url as an argument
  (rpc-call
   (string-append "<" operation-name ">"
                  "<project_url>" project-url "</project_url>"
                  "</" operation-name ">")))

(define (project-detach-xml project-url [sock-in null] [sock-out null])
  ;; Detach from a project
  (rpc-with-socket
   (string-append "<project_detach>"
                  "<project_url>" project-url "</project_url>"
                  "</project_detach>")
   sock-in sock-out))

(define (project-detach-when-done project-url)
  ;; Detach from a project when all current tasks for it are done
  (project-url-operation "project_detach_when_done" project-url))

(define (project-dont-detach-when-done project-url)
  ;; Reverse a previous request to detach from a project when all current tasks
  ;; for it are done
  (project-url-operation "project_dont_detach_when_done" project-url))

(define (project-update-xml project-url [sock-in null] [sock-out null])
  ;; Make a project update with the project server
  (project-rpc-with-socket "project_update" project-url sock-in sock-out))

(define (project-no-more-work-xml project-url [sock-in null] [sock-out null])
  ;; Request that a project receives no more tasks
  (project-rpc-with-socket "project_nomorework" project-url sock-in sock-out))

(define (project-allow-more-work-xml project-url [sock-in null] [sock-out null])
  ;; Reverse a previous request that a project receives no more tasks
  (project-rpc-with-socket "project_allowmorework" project-url sock-in
                           sock-out))

(define (project-suspend-xml project-url [sock-in null] [sock-out null])
  ;; Suspend work on a project
  (project-rpc-with-socket "project_suspend" project-url sock-in sock-out))

(define (project-resume-xml project-url [sock-in null] [sock-out null])
  ;; Resume work on a project
  (project-rpc-with-socket "project_resume" project-url sock-in sock-out))

;; Now we come to authorization. Certain RPC commands can only be run once
;; authorization has taken place. An example of this is run-benchmarks-xml.
;; Authorization occurs against a single TCP connection to the BOINC client.
;;
;; In order to perform an authorized command, three steps are necessary:
;; 1. Send auth1 to get a nonce.
;; 2. Send auth2 with a hash of the nonce concatenated with the GUI RPC password.
;; 3. Run the authorized command.
;;
;; Because authorization is granted on a connection-by-connection basis all
;; three steps must be carried out using the same socket. Because of this, auth1
;; auth2 and all authorized commands have parameters for input and ouput sockets
;; in addition to any other parameters needed to actually carry out the command.

(define (auth1-xml sock-in sock-out)
  ;; Perform the first part of GUI RPC authentication with the running BOINC
  ;; client. It will give a "nonce", which is currently a timestring.
  (rpc-call "<auth1/>" sock-in sock-out))

(define (auth2-xml nonce-hash sock-in sock-out)
  ;; Perform the second part of GUI RPC authentication with the running BOINC
  ;; client. Once this part succeeds then the connection is authorized to make
  ;; certain RPC calls that would otherwise be rejected.
  ;;
  ;; nonce-hash is the MD5 hash of the nonce retrieved by calling auth1-xml
  ;; concatenated with the GUI rpc password, which can be found in
  ;; gui_rpc_auth.cfg.
  (let* ((xml-str (string-append "<auth2><nonce_hash>"
                                 nonce-hash
                                 "</nonce_hash></auth2>")))
    (rpc-call xml-str sock-in sock-out)))    

(define (get-project-config-xml project-url [sock-in null] [sock-out null])
  ;; Get project config. Requires authorization.

  (rpc-with-socket (string-append "<get_project_config><url>" project-url "</url></get_project_config>") 
                   sock-in sock-out))

(define (get-project-config-poll-xml [sock-in null] [sock-out null])
  (rpc-with-socket "<get_project_config_poll/>" sock-in sock-out))

(define (run-benchmarks-xml [sock-in null] [sock-out null])
  ;; Stops execution of tasks. Runs hardware benchmarks on the computer running
  ;; the BOINC client. Requires authorization.
  (rpc-with-socket "<run_benchmarks />" sock-in sock-out))

(define (account-manager-rpc-xml url username password [sock-in null] [sock-out null])
  ;; Attaches to the account manager at the given url with the given username and password.
  ;; Requires authorization.
  (define xml (string-append "<acct_mgr_rpc>"
                               "<url>" url "</url>"
			       "<name>" username "</name>"
			       "<password>" password "</password>"
			     "</acct_mgr_rpc>"))
  (rpc-with-socket xml sock-in sock-out))

(define (account-manager-rpc-poll-xml [sock-in null] [sock-out null])
  (rpc-call "<acct_mgr_rpc_poll/>" sock-in sock-out)) 

(define (result-rpc-with-socket op result-name project-url [sock-in null]
                                [sock-out null])
  (rpc-with-socket
   (string-append "<" op ">"
                  "<name>" result-name "</name>"
                  "<project_url>" project-url "</project_url>"
                  "</" op ">")
   sock-in sock-out))

(define (project-rpc-with-socket op project-url [sock-in null] [sock-out null])
  (rpc-with-socket
   (string-append "<" op ">"
                  "<project_url>" project-url "</project_url>"
                  "</" op ">")
   sock-in sock-out))

(define (rpc-with-socket xml [sock-in null] [sock-out null])
  ;; Sends the given XML as an RPC call using the given input and output
  ;; sockets.
  (define-values (cin cout) (maybe-get-socket sock-in sock-out))
  (let ((result (rpc-call xml sock-in sock-out)))
    (maybe-close-socket sock-in cin cout)
    result))

(define (base-rpc-call-xml xml request-tag-name reply-end-tag [sock-in null] [sock-out null])

  (define (read-in cin [buffer ""])
    ;; Read the given input socket  until the close of an rpc reply is read.
    ;; Not much good if one is never received, though. This really needs to be
    ;; fixed.
    (define line-in (read-line cin))
    (if (string-suffix? line-in reply-end-tag)
      (string-append buffer line-in) 
      (read-in cin (string-append buffer line-in))))

  (define-values (cin cout) (maybe-get-socket sock-in sock-out))
    
  (display (string-append "<" request-tag-name ">" xml "</" request-tag-name ">\n\003") cout)
  (flush-output cout)

  (define xml-in (string-trim (read-in cin) "\u0003"))
  (maybe-close-socket sock-in cin cout)
  xml-in)
 
(define (rpc-call xml [sock-in null] [sock-out null])                      
  ;; Perform an RPC call with the given xml. The given xml should not be
  ;; wrapped in gui_rpc_request elements as these are added in here.
  (base-rpc-call-xml xml "gui_rpc_request" "</boinc_gui_rpc_reply>" sock-in sock-out)) 

  


