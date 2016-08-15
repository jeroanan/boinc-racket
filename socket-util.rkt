#lang racket

(provide maybe-get-socket
         maybe-close-socket)

(define (maybe-get-socket sock-in sock-out)
  (if (null? sock-in)
      (tcp-connect "localhost" 31416)
      (values sock-in sock-out)))

(define (maybe-close-socket sock-in cin cout)
  (let ((do-close (lambda ()
                      (close-input-port cin)
                      (close-output-port cout))))
    (if (null? sock-in)
        (do-close)
        #f)))
      
