(define-syntax-rule (xexpr-get-document-element xml-string)
  (xml->xexpr (document-element (read-xml (open-input-string xml-string)))))

(define-syntax-rule (xexpr-1st-child-element document-element)
  (caddr document-element))

(define-syntax-rule (xexpr-2nd-child-element document-element)
  (cadddr document-element))

(define-syntax-rule (xexpr-3rd-child-element document-element)
  (caddr (cddr document-element)))
