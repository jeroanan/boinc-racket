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

(provide show-attach-project-window)

(define (show-attach-project-window parent [project-url ""])

  (define min-width 200)
  (define min-height 200)

  (define dialog (new dialog%
                     [label "Attach project"]
                     [parent parent]
                     [min-width min-width]
                     [min-height min-height]))
  (send dialog show #t))
