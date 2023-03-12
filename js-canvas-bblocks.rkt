#lang racket

(provide (for-syntax document) js:elt-size)

(require "js-canvas-translator.rkt")


;; js:elt-size is a macro that generates a JavaScript function that sets the
;; height and width of a canvas element with given id to the size specified
;; in the html. Example usage: (js:elt-size 'tag)
; TODO remove the curly braces.
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


#|
(define draw-axis
  (scm->js
   (λ (id)
     (let mut ([elt ((document 'get-element-by-id) id)]
               [ctx ((elt 'get-context) '2d)]
               [w (elt 'client-width)]
               [h (elt 'client-height)])
       (ctx 'draw-path)
       ((ctx 'move-to) 0 (/ h 2))
       ((ctx 'line-to) w (/ h 2))
       (ctx 'stroke)))))
|#