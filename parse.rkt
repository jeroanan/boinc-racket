#lang racket

(require "boinc-structs.rkt")

(provide parse-host-info)
(provide parse-projects)
(provide parse-results)
(provide parse-net-stats)
(provide parse-time-stats)
(provide parse-global-preferences)
(provide parse-workunits)
(provide parse-app)
(provide parse-app-version)

(define (parse-host-info stats)
  (let ((gs (lambda (x) (get-stat-value stats x))))
    (host-info (gs 'timezone)
               (gs 'domain_name)
               (gs 'ip_addr)
               (gs 'host_cpid)
               (gs 'p_ncpus)
               (gs 'p_vendor)
               (gs 'p_model)
               (gs 'p_features)
               (gs 'p_fpops)
               (gs 'p_iops)
               (gs 'p_membw)
               (gs 'p_calculated)
               (gs 'p_vm_extensions_disabled)
               (gs 'm_nbytes)
               (gs 'm_cache)
               (gs 'm_swap)
               (gs 'd_total)
               (gs 'os_name)
               (gs 'os_version)
               (gs 'coprocs))))

(define (parse-projects stats)
    (define (f x gs gse gns)
      (project (gs 'master_url)
               (gs 'project_name)
               (gs 'symstore)
               (gs 'user_name)
               (gs 'team_name)
               (gs 'host_venue)
               (gs 'email_hash)
               (gs 'cross_project_id)
               (gs 'external_cpid)
               (gs 'cpid_time)
               (gs 'user_total_credit)
               (gs 'user_expavg_credit)
               (gs 'user_create_time)
               (gs 'rpc_seqno)
               (gs 'userid)
               (gs 'teamid)
               (gs 'hostid)
               (gs 'host_total_credit)
               (gs 'host_expavg_credit)
               (gs 'host_create_time)
               (gs 'nrpc_failures)
               (gs 'master_fetch_failures)
               (gs 'min_rpc_time)
               (gs 'next_rpc_time)
               (gs 'rec)
               (gs 'rec_time)
               (gs 'resource_share)
               (gs 'desired_disk_usage)
               (gs 'duration_correction_factor)
               (gs 'sched_rpc_pending)
               (gs 'send_time_stats_log)
               (gs 'send_job_log)
               (gs 'njobs_success)
               (gs 'njobs_error)
               (gs 'elapsed_time)
               (gs 'dont_use_dcf)
               (parse-into-two-member-struct (gse 'rsc_backoff_time) name-value 'name 'value)
               (parse-into-two-member-struct (gse 'rsc_backoff_interval) name-value 'name 'value)
               (parse-gui-urls (get-nodes (gse 'gui_urls) 'gui_url))
               (gs 'sched_priority)
               (gs 'last_rpc_time)
               (gs 'project_files_downloaded_time)
               (gs 'project_dir)))
    (accumulate-element-list stats f))

(define (parse-gui-urls stats [output (list)])
  (if (empty? stats)
      output
      (let* ((this-url (sublists-only (rest (first stats))))
             (gs (lambda (x) (get-stat-value this-url x)))
             (gu (gui-url (gs 'name)
                          (gs 'description)
                          (gs 'url)))
             (new-output (append output (list gu))))
        (if (empty? (rest stats))
            new-output
            (parse-gui-urls (rest stats) new-output)))))

(define (parse-results stats)
    (define (f x gs gse gns)
      (result (gs 'name)
                 (gs 'wu-name)
                 (gs 'version_num)
                 (gs 'plan_class)
                 (gs 'project_url)
                 (gs 'final_cpu_time)
                 (gs 'final_elapsed_time)
                 (gs 'exit_status)
                 (gs 'state)
                 (gs 'report_deadline)
                 (gs 'received_time)
                 (gs 'estimated_cpu_time_remaining)
                 (parse-active-task (gse 'active_task))))
    (accumulate-element-list stats f))

(define (parse-active-task stats)
    (let* ((gs (lambda (x) (get-stat-value stats  x))))
      (active-task (gs 'active_task_state)
                   (gs 'app_version_num)
                   (gs 'slot)
                   (gs 'pid)
                   (gs 'scheduler_state)
                   (gs 'checkpoint_cpu_time)
                   (gs 'fraction_done)
                   (gs 'current_cpu_time)
                   (gs 'elapsed_time)
                   (gs 'swap_size)
                   (gs 'working_set_size)
                   (gs 'working_set_size_smoothed)
                   (gs 'page_fault_rate)
                   (gs 'bytes_sent)
                   (gs 'bytes_received))))

(define (parse-net-stats stats)
  ;; Used to get the net-stats part of get-state
  (let ((gs (lambda (x) (get-stat-value stats x))))
    (net-stats (gs 'bwup)
               (gs 'avg_up)
               (gs 'avg_time_up)
               (gs 'bwdown)
               (gs 'avg_down) 
               (gs 'avg_time_down))))

(define (parse-time-stats stats)
  ;; Used to get the time-stats part of get-state
  (let ((gs (lambda (x) (get-stat-value stats x))))
    (time-stats (gs 'on_frac)
                (gs 'connected-frac)
                (gs 'cpu_and_network_available_frac)
                (gs 'active_frac)
                (gs 'gpu_active_frac)
                (gs 'client_start_time)
                (gs 'total_start_time)
                (gs 'total_duration)
                (gs 'total_active_duration)
                (gs 'total_gpu_active_duration)
                (gs 'now)
                (gs 'previous_uptime)
                (gs 'session_active_duration)
                (gs 'session_gpu_active_duration))))

(define (parse-global-preferences stats)
    (let ((gs (lambda (x) (get-stat-value stats x))))
      (global-preferences (gs 'source_project)
                          (gs 'mod_time)
                          (gs 'battery_charge_min_pct)
                          (gs 'battery_max_temperature)
                          (gs 'run_on_batteries)
                          (gs 'run_if_user_active)
                          (gs 'run_gpu_if_user_active)
                          (gs 'suspend_if_no_recent_input)
                          (gs 'suspend_cpu_usage)
                          (gs 'start_hour)
                          (gs 'end_hour)
                          (gs 'net_start_hour)
                          (gs 'net_end_hour)
                          (gs 'leave_apps_in_memory)
                          (gs 'confirm_before_connecting)
                          (gs 'hangup_if_dialed)
                          (gs 'dont_verify_images)
                          (gs 'work_buf_min_days)
                          (gs 'work_buf_additional_days)
                          (gs 'max_ncpus_pct)
                          (gs 'cpu_scheduling_period_minutes)
                          (gs 'disk_interval)
                          (gs 'disk_max_used_gb)
                          (gs 'disk_max_used_pct)
                          (gs 'disk_min_free_gb)
                          (gs 'vm_max_used_pct)
                          (gs 'ram_max_used_busy_pct)
                          (gs 'ram_max_used_idle_pct)
                          (gs 'idle_time_to_run)
                          (gs 'max_bytes_sec_up)
                          (gs 'max_bytes_sec_down)
                          (gs 'cpu_usage_limit)
                          (gs 'daily_xfer_limit_mb)
                          (gs 'daily_xfer_period_days)
                          (gs 'override_file_present)
                          (gs 'network_wifi_only))))

(define (parse-workunits stats)
  (define (f x gs gse gns)
    (workunit (gs 'name)
              (gs 'app_name)
              (gs 'version_num)
              (gs 'rsc_fpops_est)
              (gs 'rsc_fpops_bound)
              (gs 'rsc_memory_bound)
              (gs 'rsc_disk_bound)
              (parse-into-two-member-struct (gse 'file_ref) work-unit-file-ref 'file_name 'open_name)))
  (accumulate-element-list stats f))

(define (parse-app stats)
  (define (f x gs gse gns)
    (boinc-app
     (gs 'name)
     (gs 'user_friendly_name)
     (gs 'non_cpu_intensive)))
  (accumulate-element-list stats f))

(define (parse-app-version stats)
  (define (f x gs gse gns)
    (app-version
     (gs 'subset_sum)
     (gs 'version_num)
     (gs 'platform)
     (gs 'avg_ncpus)
     (gs 'max_ncpus)
     (gs 'flops)
     (gs 'api_version)
     (parse-app-version-fileref (gns 'file_ref))))
  (accumulate-element-list stats f))

(define (parse-app-version-fileref stats)
  (define (f x gs gse gns)
    (parse-into-two-member-struct
     x
     app-version-fileref
     'file_name
     'main_program))
  (accumulate-element-list stats f))

(define (get-stat-value stats stat-name)
  (let ((stat-entry (get-node stats stat-name)))
    (if (list? stat-entry)
        (if (< (length stat-entry) 3) "" (third stat-entry))
        "")))

(define (get-node stats stat-name)
  (if (empty? stats)
      (list)
      (if (or (empty? (first stats)) (not (list? (first stats))))
          (if (empty? (rest stats))
              (list)
              (get-node (rest stats) stat-name))
          (if (eq? (first (first stats)) stat-name)
              (first stats)
              (if (empty? (rest stats))
                  (list)
                  (get-node (rest stats) stat-name))))))

(define (parse-into-two-member-struct stats struct-type member1 member2)
    (let ((gs (lambda (x) (get-stat-value stats x))))
      (struct-type (gs member1)
                   (gs member2))))

(define (get-nodes stats stat-name [nodes (list)])  
  (if (eq? (first (first stats)) stat-name)
      (if (empty? (rest stats))
          (append nodes (list (first stats)))
          (get-nodes (rest stats) stat-name (append nodes (list (first stats)))))
      (if (empty? (rest stats))
          nodes
          (get-nodes (rest stats) stat-name nodes))))

(define (accumulate-element-list elements construct-func)

  (define (make-get-stat-value elements)
    (lambda (x) (get-stat-value elements x)))

  (define (make-get-stat-element elements)
    (lambda (x) (sublists-only (get-node elements x))))
  
  (define (make-get-child-nodes elements)
    (lambda (x) (get-nodes elements x)))
  
  (define (do-it elements [output (list)])
    (if (empty? elements)
        output
        (let* ((this-element (sublists-only (first elements)))
               (gs (make-get-stat-value this-element))
               (gse (make-get-stat-element this-element))
               (gns (make-get-child-nodes this-element))
               (new-output (append output (list (construct-func this-element gs gse gns)))))
          (if (empty? (rest elements))
              new-output
              (do-it (rest elements) new-output)))))
  (do-it elements))

(define (sublists-only element)
  (if (and (list? element) (not (empty? element)))
      (filter (lambda (x)
                (and (list? x)
                     (not (empty? x))))
              (cddr element))
      (list)))