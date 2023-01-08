#lang racket

(provide canvas:style)

(require json)

;; TODO provide sane defaults (as a fraction of the page size)
(define (canvas:style #:id [id "canvas"] #:width [w 0] #:height [h 0])
  (let* ([params #hash((margin           . "20px auto")
                       (display          . "flex")
                       (align-items      . "center")
                       (border           . "1px solid black")
                       (background-color . "lightyellow"))]
         [params (if (> w 0) (hash-set params 'width (format "~a%" w)) params)]
         [params (if (> h 0) (hash-set params 'height (format "~a%" h)) params)])
    (let ([content
           (with-output-to-string
             (lambda () (write-json params)))])
      (format "~a ~a" id
              (string-replace (string-replace content "\"" "") "," ";")))))

;(display (canvas:style #:width 40 #:height 80))
