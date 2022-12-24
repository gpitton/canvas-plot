#lang racket

(provide canvas:style)


;; TODO provide sane defaults (as a fraction of the page size)
(define (canvas:style #:width w #:height h)
  (format "canvas {\nwidth: ~a%;\nheight: ~a%;\nmargin: 20px auto;
display: flex;\nalign-items: center;\nborder: 1px solid black;
background-color: lightyellow;\n}\n" w h))

;(display (canvas:style #:width 40 #:height 80))
