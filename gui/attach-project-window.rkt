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

(require racket/gui/base)

(require "../boinc-commands.rkt")
(require "../boinc-structs.rkt")
(require "widget-tools/button-tools.rkt")
(require "widget-tools/caution-box.rkt")

(provide show-attach-project-window)

(define (show-attach-project-window parent [project-url ""])

  (define min-width 600)
  (define min-height 200)

  (define dialog (new dialog%
                     [label "Attach project"]
                     [parent parent]
                     [min-width min-width]
                     [min-height min-height]))

  (define project-url-textbox (new text-field%
                                   [label "Project URL"]
                                   [parent dialog]))
  (send project-url-textbox set-value project-url)
  
  (define email-address-textbox (new text-field%
                                [label "Email address"]
                                [parent dialog]))

  (define password-textbox (new text-field%
                                [label "Password"]
                                [parent dialog]
                                [style (list 'single 'password)]))

  (define button-container (new horizontal-pane%
                                [parent dialog]
                                [alignment (list 'right 'center)]))

  (define unauthorized-message (make-caution-box dialog
                                                  "Authorization with BOINC failed. Check your GUI RPC password settings."
                                                  "Authorization failed"))

  (define (show-caution-message message)
    (send (make-caution-box dialog message "Error") show #t))

  (define (ok-button-clicked)
    (define project-url (send project-url-textbox get-value))
    (define email-address (send email-address-textbox get-value))
    (define password (send password-textbox get-value))
    
    (with-handlers
      ([(lambda (v) (equal? v 'unauthorized))
        (lambda (v) (send unauthorized-message show #t))])

      (define lookup-result (lookup-account project-url email-address password))
      (cond
        [(error-message? lookup-result) (show-caution-message
                                         (error-message-message lookup-result))])
      (display lookup-result)
      (display "\n")
      #f)
    #f)

  (define button-maker (get-simple-button-maker button-container))
  (define cancel-button (button-maker "Cancel" (lambda () (send dialog show #f))))
  (define ok-button (button-maker "OK" ok-button-clicked))
  
                                   
  (send dialog show #t))
