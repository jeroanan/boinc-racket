#lang racket

(require racket/unix-socket)
(require "socket-util.rkt")
(provide exchange-versions-xml)
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
(provide auth1-xml
         auth2-xml)

(define (exchange-versions-xml)
  ;; makes an exchange_versions RPC call
  (rpc-call "<exchange_versions/>"))

(define (get-cc-status-xml)
  ;; makes a cc-status RPC call
  (rpc-call "<get_cc_status/>"))

(define (get-cc-config-xml)
  ;; Gets client configuration. Requires authorization.
  (rpc-call "<get_cc_config />"))

(define (get-host-info-xml)
  ;; Gets host info
  (rpc-call "<get_host_info />"))

(define (get-newer-version-xml)
  ;; Checks whether a newer version is available
  (rpc-call "<get_newer_version />"))
  
(define (get-project-status-xml)
  ;; Gets project status
  (rpc-call "<get_project_status />"))

(define (get-results-xml [active-only #f])  
  ;; Get results. If active-only then all results, otherwise active ones only.
  (rpc-call
   (string-append "<get_results><active_only>" (if active-only "true" "false" ) "</active_only></get_results>")))

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

(define (project-op op result-name project-url)
  ;; Makes rpc call for a generic project operation -- see following functions
  (rpc-call
   (string-append "<" op ">"
                  "<name>" result-name "</name>"
                  "<project_url>" project-url "</project-url>"
                  "</" op ">")))

(define (abort-result result-name project-url)
  ;; Abort result
  (project-op "abort_result" result-name project-url))

(define (suspend-result result-name project-url)
  ;; Suspend result
  (project-op "suspend_result" result-name project-url))

(define (resume-result result-name project-url)
  ;; Resume result
  (project-op "resume_result" result-name project-url))

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

(define (get-notices-xml)
  ;; Get notices. Requires authorization.
  (rpc-call "<get_notices />"))

(define (get-notices-public-xml)
  ;; Get public notices. Does not require authorization.
  (rpc-call "<get_notices_public />"))

(define (get-account-manager-info-xml)
  ;; Get account manager info. Requires authorization.
  (rpc-call "<acct_mgr_info />"))

(define (get-global-prefs-file-xml)
  ;; Get global preferences file xml. Requires authorization.
  (rpc-call "<get_global_prefs_file />"))

(define (get-project-init-status-xml)
  ;; Get project init status xml. Requires authorization.
  (rpc-call "<get_project_init_status />"))

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

(define (lookup-account project-url email-address password-hash)
  ;; Look up an existing project account. Requires authorization.
  (rpc-call
   (string-append "<lookup_account>"
                  "<url>" project-url "</url>"
                  "<email_addr>" email-address "</email_addr>"
                  "<passwd_hash>" password-hash "</passwd_hash>"
                  "<ldap_auth>0</ldap_auth>"
                  "</lookup_account>")))

(define (lookup-account-poll)
  ;; Poll the progress of project account lookup. Requires authorization.
  (rpc-call "<lookup_account_poll />"))

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

(define (project-attach project-url authenticator)
  ;; Attach to a project
  (rpc-call
   (string-append "<project_attach>"
                  "<use_config_file>false</use_config_file>"
                  "<project_url>" project-url "</project-url>"
                  "<authenticator>" authenticator "</authenticator>"
                  "<project_name>" project-url "</project_name>"
                  "<project_attach>")))

(define (project-url-operation operation-name project-url)
  ;; Perform a generic operation on a project when that project requires only a project url as an argument
  (rpc-call
   (string-append "<" operation-name ">"
                  "<project_url>" project-url "</project-url>"
                  "</" operation-name ">")))

(define (project-detach project-url)
  ;; Detach from a project
  (project-url-operation "project_detach" project-url))

(define (project-detach-when-done project-url)
  ;; Detach from a project when all current tasks for it are done
  (project-url-operation "project_detach_when_done" project-url))

(define (project-dont-detach-when-done project-url)
  ;; Reverse a previous request to detach from a project when all current tasks for it are done
  (project-url-operation "project_dont_detach_when_done" project-url))

(define (project-update project-url)
  ;; Make a project update with the project server
  (project-url-operation "project_update" project-url))

(define (project-no-more-work project-url)
  ;; Request that a project receives no more tasks
  (project-url-operation "project_nomorework" project-url))

(define (project-allow-more-work project-url)
  ;; Reverse a previous request that a project receives no more tasks
  (project-url-operation "project_allowmorework" project-url))

(define (project-suspend project-url)
  ;; Suspend work on a project
  (project-url-operation "project_suspend" project-url))

(define (project-resume project-url)
  ;; Resume work on a project
  (project-url-operation "project_resume" project-url))

(define (auth1-xml sock-in sock-out)
  (rpc-call "<auth1/>" sock-in sock-out))

(define (auth2-xml nonce-hash sock-in sock-out)
  (let* ((xml-str (string-append "<auth2><nonce_hash>" nonce-hash "</nonce_hash></auth2>")))
    (rpc-call xml-str sock-in sock-out)))    

(define (rpc-call xml [sock-in null] [sock-out null])
                      
  ;; Perform an RPC call with the given xml. The given xml should not be wrapped in gui_rpc_request elements.
  (define (get-gui-rpc-request-xml inner-xml)
    ;; Construct gui_rpc_request xml. It ends with a new line and character 0x03.
    (string-append "<gui_rpc_request>" inner-xml "</gui_rpc_request>\n\003"))

  (define (read-in cin [buffer ""])
    ; Read the given input socket  until the close of an rpc reply is read. Not much good if one is never received, though
    (let ((line-in (read-line cin)))
      (if (string-suffix? line-in "</boinc_gui_rpc_reply>")
          (string-append buffer line-in) (read-in cin (string-append buffer line-in)))))

  (define-values (cin cout) (maybe-get-socket sock-in sock-out))
    
  (display (get-gui-rpc-request-xml xml) cout)
  (flush-output cout)
  (let ((xml-in (read-in cin)))    
   (maybe-close-socket sock-in cin cout)
    xml-in))
