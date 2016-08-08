#lang racket

(provide (all-defined-out))

(require xml)

(define (xexpr-get-document-element xml-string)
  (xml->xexpr (document-element (read-xml (open-input-string xml-string)))))

(define (sublists-only element)
  (if (and (list? element) (not (empty? element)))
      (filter (lambda (x)
                (and (list? x)
                     (not (empty? x))))
              (cddr element))
      (list)))

(define (get-node stats stat-name)
  (if (empty? stats)
      (list)
      (if (or (empty? (first stats)) (not (list? (first stats))))
          (if (empty? (rest stats))
              (list)
              (get-node (rest stats) stat-name))
          (if (eq? (first (first stats)) stat-name)
              (first stats)
              (if (empty? (rest stats))
                  (list)
                  (get-node (rest stats) stat-name))))))

(define (get-nodes stats stat-name [nodes (list)])  
  (if (eq? (first (first stats)) stat-name)
      (if (empty? (rest stats))
          (append nodes (list (first stats)))
          (get-nodes (rest stats) stat-name (append nodes (list (first stats)))))
      (if (empty? (rest stats))
          nodes
          (get-nodes (rest stats) stat-name nodes))))

(define (get-stat-value stats stat-name)
  (let ((stat-entry (get-node stats stat-name)))
    (if (list? stat-entry)
        (if (< (length stat-entry) 3) "" (third stat-entry))
        "")))

(define (get-main-node f node-id)
  (let* ((root-xml (xexpr-get-document-element (f)))
         (main-node (sublists-only (get-node (cddr root-xml) node-id))))
    main-node))

(define (accumulate-element-list elements construct-func)
  
  (define (make-get-stat-value elements)
    (lambda (x) (get-stat-value elements x)))
  
  (define (make-get-stat-element elements)
    (lambda (x) (sublists-only (get-node elements x))))
  
  (define (make-get-child-nodes elements)
    (lambda (x) (get-nodes elements x)))
  
  (define (do-it elements [output (list)])
    (if (empty? elements)
        output
        (let* ((this-element (sublists-only (first elements)))
               (gs (make-get-stat-value this-element))
               (gse (make-get-stat-element this-element))
               (gns (make-get-child-nodes this-element))
               (new-output (append output (list (construct-func this-element gs gse gns)))))
          (if (empty? (rest elements))
              new-output
              (do-it (rest elements) new-output)))))
  (do-it elements))
