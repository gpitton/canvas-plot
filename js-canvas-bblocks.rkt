#lang racket

(provide make-headers make-request (for-syntax document)
         js:elt-size js:draw-axis js:draw-point js:scatter-1d)

(require "js-canvas-translator.rkt")


;; Utility to generate code to create a new header bound to the variable
;; name var.
(define (make-headers var)
  (format "const ~a = new Headers();" var))


;; Utility to generate code to create a new request to fetch data from
;; a server. The request is bound to a variable with name var.
(define (make-request var hdr host port)
  (format "const ~a = new Request('http://~a:~a',\n{method: 'GET', action: '/', headers: ~a});"
          var host port hdr))


;; js:elt-size is a macro that generates a JavaScript function that sets the
;; height and width of a canvas element with given id to the size specified
;; in the html. Example usage: (js:elt-size 'tag)
(define-syntax js:elt-size
  (syntax-rules ()
    [(_ id)
     (scm->js
      (let mut ([elt ((document 'get-element-by-id) id)]
                [ctx ((elt 'get-context) '2d)]
                [w (elt 'client-width)]
                [h (elt 'client-height)])
        (set! (elt 'width) w)
        (set! (elt 'height) h)))]))


;; js:draw-axis is a macro that generates a JavaScript function that draws
;; the horizontal axis as a line striking through the plot area at half
;; height.
(define-syntax js:draw-axis
  (syntax-rules ()
    [(_)
     (scm->js
      (let ([draw-axis
             (λ (id)
               (let ([elt ((document 'get-element-by-id) id)]
                     [w (elt 'client-width)]
                     [h (elt 'client-height)])
                 void)
               (let mut ([ctx ((elt 'get-context) '2d)])
                 ((ctx 'draw-path))
                 ((ctx 'move-to) 0 (/ h 2))
                 ((ctx 'line-to) w (/ h 2))
                 ((ctx 'stroke))))])
        void))]))


;; js:draw-point is a macro that generates a JavaScript function that
;; draws a single point as a filled circle at the specified coordinates.
(define-syntax js:draw-point
  (syntax-rules ()
    [(_)
     (scm->js
      (let ([draw-point
             (λ (ctx x y r)
               ((ctx 'begin-path))
               ((ctx 'arc) x y r 0 (* 2 (Math 'PI)) #t)
               ((ctx 'fill)))])
        void))]))


;; js:scatter-1d is a macro that generates a JavaScript function that
;; draws a sequence of points at their respective height, on an equally
;; spaced 1-dimensional grid.
(define-syntax js:scatter-1d
  (syntax-rules ()
    [(_)
     (scm->js
      (let ([scatter-1d
             (λ (id ys)
               (let ([elt ((document 'get-element-by-id) id)]
                     [w (elt 'client-width)]
                     [h (elt 'client-height)])
                 void)
               (let mut ([ctx ((elt 'get-context) '2d)])
                 (draw-axis ctx))
               ;; TODO implement this:
               ;;if (ys===undefined) console.log('undefined detected.');
               (let ([n (ys 'length)]
                     [dx (/ w (- n 1))])
                 (for (i (in-range 0 n incr))
                   (let ([x (* i dx)]
                         [y (* h (ref ys i))])
                     (draw-point ctx x y 2)))))])))]))

