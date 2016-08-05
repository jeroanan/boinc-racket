(struct get-state-result
  (host-info
   net-stats
   time-stats
   projects
   workunits
   results
   platform-name
   core-client-major-version
   core-client-minor-version
   core-client-release
   executing-as-daemon
   platform
   global-preferences
   apps
   app-versions))

(struct host-info
  (timezone
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

(struct net-stats
  (bwup
   avg-up
   avg-time-up
   bwdown
   avg-down
   avg-time-down))

(struct time-stats
  (on-frac
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

(struct global-preferences
  (source-project
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

(struct workunit
  (name
   app-name
   version-num
   rsc-fpops-est
   rsc-fpops-bound
   rsc-memory-bound
   rsc-disk-bound
   file-ref))

(struct work-unit-file-ref (file-name open-name))

(struct result
  (name
   wu-name
   version-num
   plan-class
   project-url
   final-cpu-time
   final-elapsed-time
   exit-status
   state
   report-deadline
   received-time
   estimated-cpu-time-remaining
   active-task))

(struct active-task
  (active-task-state
   app-version-num
   slot
   pid
   scheduler-state
   checkpoint-cpu-time
   fraction-done
   current-cpu-time
   elapsed-time
   swap-size
   working-set-size
   working-set-size-smoothed
   page-fault-rate
   bytes-sent
   bytes-received))

(struct project
  (master-url
   project-name
   symstore
   user-name
   team-name
   host-venue
   email-hash
   cross-project-id
   external-cpid
   cpid-time
   user-total-credit
   user-expavg-credit
   user-create-time
   rpc-seqno
   userid
   teamid
   hostid
   host-total-credit
   host-expavg-credit
   host-create-time
   nrpc-failures
   master-fetch-failures
   min-rpc-time
   next-rpc-time
   rec
   rec-time
   resource-share
   desired-disk-usage
   duration-correction-factor
   sched-rpc-pending
   send-time-stats-log
   send-job-log
   njobs-success
   njobs-error
   elapsed-time
   dont-use-dcf
   rsc-backoff-time
   rsc-backoff-interval
   gui-urls
   sched-priority
   last-rpc-time
   project-files-downloaded-time
   project-dir))

(struct name-value (name value))

(struct gui-url (name description url))

(struct boinc-app (name user-friendly-name non-cpu-intensive))

(struct app-version (subset-sum
                     version-num
                     platform
                     avg-ncpus
                     max-ncpus
                     flops
                     api-version
                     file-ref))

(struct app-version-fileref (file-name main-program))