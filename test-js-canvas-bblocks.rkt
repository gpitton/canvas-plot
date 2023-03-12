#lang racket

(require "js-canvas-bblocks.rkt")
(require rackunit rackunit/text-ui)

(define-test-suite js-canvas-bblocks
  (test-case
   "elt-size"
   (check-equal? (js:elt-size 'tag)
                 "{\nlet elt = document.getElementById('tag');\nlet ctx = \
elt.getContext('2d');\nlet w = elt.clientWidth;\nlet h = elt.clientHeight;\n\
elt.width = w;\nelt.height = h;\n}\n\n"))
  (test-case
   "draw-axis"
   (check-equal? (js:draw-axis 'tag)
                 "{\nlet elt = document.getElementById('tag');\nlet ctx = \
elt.getContext('2d');\nlet w = elt.clientWidth;\nlet h = elt.clientHeight;\n\
let h2 = h / 2;\nctx.drawPath();\nctx.moveTo(0, h2);\nctx.lineTo(w, h2);\n\
ctx.stroke();\n}\n\n")))
                 

(run-tests js-canvas-bblocks)