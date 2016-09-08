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

(require "button-tools.rkt")

(provide make-caution-box)

(define (make-caution-box parent message [title "Caution"])

  (define dialog (new dialog%
                     [label title]
                     [parent parent]
                     [min-width 300]
                     [min-height 50]))

  (define message-container (new horizontal-pane%
                                 [parent dialog]))

  (define button-container (new horizontal-pane%
                                [parent dialog]
                                [alignment (list 'center 'center )]))

  (define caution-icon (new message%
                            [parent message-container]
                            [label 'caution]))

  (define caution-message (new message%
                               [parent message-container]
                               [label message]))

  (define button-maker (get-simple-button-maker button-container))
  (define ok-button (button-maker "&OK" (lambda()
                                          (send dialog show #f))))
  dialog)
    


    
                                 
