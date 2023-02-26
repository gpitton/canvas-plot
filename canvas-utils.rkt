#lang racket

(provide canvas:style html:style)

(require json)

;; to-json formats a string obtained by formatting a hash table with write-json.
;; It is useful to generate a string that can be parsed as a CSS or JavaScript
;; source object.
(define (to-json str)
  (string-replace
   ;; Strip all occurrences of \"
   (string-replace str "\"" "")
   ;; Replace all commas with semicolons.
   "," ";"))


;; This is a default text style that should be reasonably readable on most browsers.
(define (html:style)
  (let ([params
         #hash((font-size . "1.6rem")
               (padding   . "2%")
               (margin    . "2%"))])
    (let ([content
           (with-output-to-string
             (lambda () (write-json params)))])
      (format "html ~a" (to-json content)))))


;; TODO provide sane defaults (as a fraction of the page size)
(define (canvas:style #:id [id "canvas"] #:width [w 0] #:height [h 0])
  (let* ([params
          #hash((margin           . "1% auto")
                (display          . "flex")
                (align-items      . "center")
                (border           . "2px solid black")
                (background-color . "lightyellow"))]
         [params (if (> w 0) (hash-set params 'width (format "~a%" w)) params)]
         [params (if (> h 0) (hash-set params 'height (format "~a%" h)) params)])
    (let ([content
           (with-output-to-string
             (lambda () (write-json params)))])
      (format "~a ~a" id (to-json content)))))

;(display (canvas:style #:width 40 #:height 80))
