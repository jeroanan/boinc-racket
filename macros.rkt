#lang racket

;; see https://mebdev.blogspot.co.uk/2011/06/brief-intro-to-writing-macros-in-racket.html

(provide while
         aif)

(define-syntax (while stx-obj) 
  (syntax-case stx-obj (while)
    [(_ test body)
     #`(begin (define (loop)
                (if test
                    (begin body
                           (loop))
                    (display "")))
              (loop))]))

(define-syntax (aif stx-obj)
  (syntax-case stx-obj (aif)
    [(_ test body)
     #`(if test
           body
           (void))]))


