#lang racket

(require json)

(provide get-gui-rpc-auth-file-location)

(define (get-setting setting-name)
  (define config-json (string->jsexpr (file->string "config.json")))
  (hash-ref config-json setting-name))

(define (get-gui-rpc-auth-file-location) (get-setting 'gui_rpc_auth_file_location))

