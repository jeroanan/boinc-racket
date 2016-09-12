#lang racket

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

(require racket/gui/base)

(provide add-list-column
         new-list-box
         set-listbox-data
         get-listbox-selected-data)

(define (set-list-column-items list-ctrl column-no list-items [n 0])
  ;; For the given list control and column number, set the contents.
  (define (do-add)
    (send list-ctrl set-string n (first list-items) column-no)
    (set-list-column-items list-ctrl column-no (rest list-items) (+ n 1)))  
  (if (empty? list-items) #t (do-add)))

(define (add-list-column list-ctrl label width contents)
  ;; Append a new column to a list box with the given label, width and contents
  (define number-of-columns (length (send list-ctrl get-column-labels)))
  (send list-ctrl append-column label)
  (send list-ctrl set-column-width number-of-columns width 0 1000000)
  (set-list-column-items list-ctrl number-of-columns contents))

(define (new-list-box parent min-width choices)
  (new list-box%
       [parent parent]
       [choices choices]
       [style (list 'single
                    'variable-columns
                    'column-headers
                    'clickable-headers)]
       [min-width min-width]
       [stretchable-width #t]
       [stretchable-height #t]
       [label #f]))

(define (set-listbox-data list-box data)  
  (define (add-data data counter)
    (define (do-add)
      (send list-box set-data counter (first data))
      (add-data (rest data) (+ counter 1)))

    (if (empty? data)
        #f
        (do-add)))
  (add-data data 0))

(define (get-listbox-selected-data list-box)
  (define selections (send list-box get-selections))
  (if (empty? selections)
      #f
      (send list-box get-data (first selections))))
