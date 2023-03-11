#lang racket

(provide canvas:style html:body:style)

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
(define (html:body:style)
  (let ([params
         #hash((font-size . "1.6rem")
               (color . "#222831")
               (background . "#eeeeee")
               (line-height . "1.0rem")
               (padding   . "2%")
               (margin    . "2%"))])
    (let ([content
           (with-output-to-string
             (lambda () (write-json params)))])
      (format "body ~a" (to-json content)))))

;; TODO write a default style for h1, h2, h3 (maybe with line-height: "1.6rem"

;; canvas:style is an acceptable default for the style of scatter plots.
(define (canvas:style #:id [id "canvas"])
  (let ([params
         #hash((margin           . "1% auto")
               (display          . "flex")
               (align-items      . "center")
               (border-color     . "#393e46")
               (border           . "2px solid")
               (background-color . "lightyellow"))])
    (let ([content
           (with-output-to-string
             (lambda () (write-json params)))])
      (format "~a ~a" id (to-json content)))))

;(display (canvas:style #:width 40 #:height 80))
