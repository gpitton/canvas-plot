#lang racket

(provide canvas:style)

(require json)

;; TODO provide sane defaults (as a fraction of the page size)
(define (canvas:style #:id [id "canvas"] #:width w #:height h)
  (let ([content
         (with-output-to-string
           (lambda () (write-json
                       `#hash((width            . ,(format "~a%" w))
                              (height           . ,(format "~a%" h))
                              (margin           . "20px auto")
                              (display          . "flex")
                              (align-items      . "center")
                              (border           . "1px solid black")
                              (background-color . "lightyellow")))))])
    (format "~a ~a" id
            (string-replace (string-replace content "\"" "") "," ";"))))

;(display (canvas:style #:width 40 #:height 80))
