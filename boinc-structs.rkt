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

(provide (all-defined-out))

;; This module provides structs that are used by parse.rkt contain the reuslts
;; of RPC calls.

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
   active-tasks
   suspended-via-gui?)) 

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
   project-dir
   suspended-via-gui?
   dont-request-more-work?))

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

(struct disk-usage (disk-total
                    disk-free
                    disk-boinc
                    disk-allowed
                    projects))

(struct disk-usage-project (master-url disk-usage))

(struct cc-status
  (network-status
   ams-password-error
   task-suspend-reason
   task-mode
   task-mode-perm
   task-mode-delay
   gpu-suspend-reason
   gpu-mode
   gpu-mode-perm
   gpu-mode-delay
   network-suspend-reason
   network-mode
   network-mode-perm
   network-mode-delay
   disallow-attach
   simple-gui-only
   max-event-log-lines))

(struct daily-transfer (tx-when up down))
      
(struct message (project pri seqno body msg-time))

(struct screensaver-task (suspend-reason results))
                          
(struct simple-gui-info (projects results))

(struct project-statistics (master-url daily-statistics))

(struct daily-statistics (day
                          user-total-credit
                          user-expavg-credit
                          host-total-credit
                          host-expavg-credit))

(struct available-project (name
                           url
                           general-area
                           specific-area
                           description
                           home
                           platforms
                           image
                           summary))

(struct success ([success #:auto])
  #:auto-value #t)

(struct error-message (message))

(struct project-authenticator (authenticator))

(struct notice (title
                description
		create-time
		arrival-time
		is-private?
		project-name
		category
		link
		seqno))

(struct file-transfer (project-url
                       project-name
		       name
		       nbytes ;; total size of transfer
		       max-nbytes ;; bytes downloaded so far
		       status
		       num-retries        ;; In the XML this field and on are
		       first-request-time ;; given in a persistent_file_xfer
		       next-request-time  ;; element. However since it seems
		       time-so-far        ;; there is only ever one, I am just
		       last-bytes-transferred ;; including them here.
		       is-upload))

