#lang racket
(require json)
(require xml)
(require web-server/servlet)
(require web-server/servlet-env)
(require "canvas-utils.rkt")


(define (serve-num req)
    (let ([rands (for/list ([i (range 100)]) (random))])
        (response/output
            (lambda (op) (write-json rands op))
            #:mime-type #"text/plain; charset=utf-8"
            #:headers (list (make-header #"Access-Control-Allow-Origin" #"*")
                            (make-header #"Content-Type" #"application/json")))))

;; serve-num-2d serves an array of arrays: [xs, ys], where xs and ys are
;; themselves arrays.
(define (serve-num-2d req)
    (let* ([xs (for/list ([i (range 100)]) (random))]
           [ys (for/list ([i (range 100)]) (random))]
           [rands (list xs ys)])
         (response/output
             (lambda (op) (write-json rands op))
             #:mime-type #"text/plain; charset=utf-8"
             #:headers (list (make-header #"Access-Control-Allow-Origin" #"*")
                             (make-header #"Content-Type" #"application/json")))))

(define (fill-template title body)
  `(html (head (title ,title))
         (body ,@body)))

;; TODO find a better way to generate javascript
(define (serve-page req)
  (let* ((doctype "<!DOCTYPE html>\n")
         (script-str (js:fetch-scatter 'scatter #:port 8000))
         (script-2d-str (js:fetch-scatter-2d 'scatter-2d #:port 8002))
         (body `((p "We will use canvas to draw a scatter plot (think of a time series)")
                 (canvas ((id "scatter")))
                 (style ,(canvas:style #:width 40 #:height 80))
                 (script ,script-str)
                 (p "Below, we also send the x-coordinates for the scatter plot.")
                 (canvas ((id "scatter-2d")))
                 (script ,script-2d-str))))
    (response/output
     (lambda (op) (begin
                      (display doctype op)
                      (parameterize ([current-unescaped-tags (cons 'id html-unescaped-tags)])
                          (write-xexpr (fill-template "Experiment with canvas" body) op)))))))

;; main loop
(define text-server
  (thread (lambda ()
            (serve/servlet serve-num
                           #:port 8000
                           #:servlet-path "/"
                           #:command-line? #t))))

(define text-server-2d
  (thread (lambda ()
            (serve/servlet serve-num-2d
                           #:port 8002
                           #:servlet-path "/"
                           #:command-line? #t))))

(define http-server
  (thread (lambda ()
            (serve/servlet serve-page
                           #:port 8001
                           #:servlet-path "/"
                           #:command-line? #t))))

(sleep 20)
(kill-thread text-server)
(kill-thread http-server)