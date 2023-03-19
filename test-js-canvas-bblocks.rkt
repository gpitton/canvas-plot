#lang racket

(require "js-canvas-bblocks.rkt")
(require rackunit rackunit/text-ui)

(define-test-suite js-canvas-bblocks
  (test-case
   "utilities"
   (check-equal? (make-headers 'hdr)
                  "const hdr = new Headers();")
   (check-equal? (make-request 'r 'hdr "localhost" 8000)
                 "const r = new Request('http://localhost:8000',\n\
{method: 'GET', action: '/', headers: hdr});"))
  (test-case
   "elt-size"
   (check-equal? (js:elt-size 'tag)
                 "let elt = document.getElementById('tag');\nlet ctx = \
elt.getContext('2d');\nlet w = elt.clientWidth;\nlet h = elt.clientHeight;\n\
elt.width = w;\nelt.height = h;\n"))
  (test-case
   "draw-axis"
   (check-equal? (js:draw-axis)
                 "function drawAxis(id) {\nconst elt = document.getElementById(id);\
\nconst w = elt.clientWidth;\nconst h = elt.clientHeight;\nconst h2 = h / 2;\n\
let ctx = elt.getContext('2d');\nctx.drawPath();\nctx.moveTo(0, h2);\
\nctx.lineTo(w, h2);\nctx.stroke();\n}\n"))
  (test-case
   "draw-point"
   (check-equal? (js:draw-point)
                 "function drawPoint(ctx, x, y, r)\
 {\nctx.beginPath();\nctx.arc(x, y, r, 0, 2 * Math.PI, true);\
\nctx.fill();\n}\n")))


(run-tests js-canvas-bblocks)