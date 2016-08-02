#lang racket

(require racket/include)
(include "boinc-xml.rkt")
(include "xexpr-macros.rkt")

(define (sublists-only element)
  (filter (lambda (x) (list? x)) (cddr element)))

(struct get-state-result (host-info
                          net-stats
                          time-stats
                          platform-name
                          core-client-major-version
                          core-client-minor-version
                          core-client-release
                          executing-as-daemon
                          platform
                          global-preferences))

(struct host-info (timezone
                   domain-name
                   ip-addr
                   host-cpid
                   p-ncpus
                   p-vendor
                   p-model
                   p-features
                   p-fpops
                   p-iops
                   p-membw
                   p-calculated
                   p-vm-extensions-disabled
                   m-nbytes
                   m-cache
                   m-swap
                   d-total
                   os-name
                   os-version
                   coprocs))

(struct net-stats (bwup
                   avg-up
                   avg-time-up
                   bwdown
                   avg-down
                   avg-time-down))

(struct time-stats (on-frac
                    connected-frac
                    cpu-and-network-available-frac
                    active-frac
                    gpu-active-frac
                    client-start-time
                    total-start-time
                    total-duration
                    total-active-duration
                    total-gpu-active-duration
                    now
                    previous-uptime
                    session-active-duration
                    session-gpu-active-duration))

(struct global-preferences (source-project
                            mod-time
                            battery-charge-min-pct
                            battery-max-temperature
                            run-on-batteries
                            run-if-user-active
                            run-gpu-if-user-active
                            suspend-if-no-recent-input
                            suspend-cpu-usage
                            start-hour
                            end-hour
                            net-start-hour
                            net-end-hour
                            leave-apps-in-memory
                            confirm-before-connecting
                            hangup-if-dialed
                            dont-verify-images
                            work-buf-min-days
                            work-buf-additional-days
                            max-ncpus-pct
                            cpu-scheduling-period-minutes
                            disk-interval
                            disk-max-used-gb
                            disk-max-used-pct
                            disk-min-free-gb
                            vm-max-used-pct
                            ram-max-used-busy-pct
                            ram-max-used-idle-pct
                            idle-time-to-run
                            max-bytes-sec-up
                            max-bytes-sec-down
                            cpu-usage-limit
                            daily-xfer-limit-mb
                            daily-xfer-period-days
                            override-file-present
                            network-wifi-only))

(define (get-node stats stat-name [nodes (list)])
  (if (eq? (first (first stats)) stat-name)
      (first stats)
      (if (empty? (rest stats))
          ""
          (get-node (rest stats) stat-name))))

(define (get-stat-value stats stat-name)
  (let ((stat-entry (get-node stats stat-name)))
    (if (list? stat-entry)
        (if (< (length stat-entry) 3) "" (third stat-entry))
        "")))

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

(define (get-state)

  (define (parse-net-stats stats)
    (let ((gs (lambda (x) (get-stat-value stats x))))
      (net-stats (gs 'bwup)
                 (gs 'avg_up)
                 (gs 'avg_time_up)
                 (gs 'bwdown)
                 (gs 'avg_down) 
                 (gs 'avg_time_down))))

  (define (parse-time-stats stats)
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

  (let* ((state-xml (xexpr-get-document-element (get-state-xml)))
         (client-state (cddr (get-node (cddr state-xml) 'client_state)))
         (gse (lambda (x) (sublists-only (get-node client-state x))))
         (gses (lambda (x) (third (get-node client-state x))))
         (host-info (parse-host-info (gse 'host_info)))
         (net-stats (parse-net-stats (gse 'net_stats)))
         (time-stats (parse-time-stats (gse 'time_stats)))
         (platform-name (gses 'platform_name))
         (core-client-major-version (gses 'core_client_major_version))
         (core-client-minor-version (gses 'core_client_minor_version))
         (core-client-release (gses 'core_client_release))
         (executing-as-daemon (gses 'executing_as_daemon))
         (platform (gses 'platform))
         (global-preferences (parse-global-preferences (gse 'global_preferences)))
         (result (get-state-result
                  host-info
                  net-stats
                  time-stats
                  platform-name
                  core-client-major-version
                  core-client-minor-version
                  core-client-release
                  executing-as-daemon
                  platform
                  global-preferences)))
    client-state))

(define (get-host-info)
  (let* ((host-info-xml (xexpr-get-document-element (get-host-info-xml)))
         (host-info-node (sublists-only (cddr (get-node (cddr host-info-xml) 'host_info))))
         (result (parse-host-info host-info-node)))
    result))

(define (get-disk-usage)
  (let* ((disk-usage-xml (xexpr-get-document-element (get-disk-usage-xml))))
    disk-usage-xml))
