#lang racket

(require "js-canvas-translator.rkt")
(require rackunit rackunit/text-ui)

(define-test-suite js-canvas-translator
  (test-case
   "variable definition"
   (check-equal? (scm->js
                  (let ([v 42]) void))
                 "const v = 42;")
   (check-equal? (scm->js
                  (let mut ([v s42]) void))
                 "let v = s42;"))
  (test-case
   "method call"
   (check-equal? (scm->js
                  (let ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "const elt = document.getElementById('fig-1');")))

(run-tests js-canvas-translator)