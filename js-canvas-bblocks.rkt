#lang racket

(provide (for-syntax document) js:elt-size js:draw-axis)

(require "js-canvas-translator.rkt")

;; js:elt-size is a macro that generates a JavaScript function that sets the
;; height and width of a canvas element with given id to the size specified
;; in the html. Example usage: (js:elt-size 'tag)
; TODO remove the curly braces, here and thoughout.
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
; TODO indent function body.
(define-syntax js:draw-axis
  (syntax-rules ()
    [(_)
     (scm->js
      (let ([draw-axis
             (λ (id)
               (let mut ([elt ((document 'get-element-by-id) id)]
                         [ctx ((elt 'get-context) '2d)]
                         [w (elt 'client-width)]
                         [h (elt 'client-height)]
                         [h2 (/ h 2)])
                 ((ctx 'draw-path))
                 ((ctx 'move-to) 0 h2)
                 ((ctx 'line-to) w h2)
                 ((ctx 'stroke))))])
        void))]))
