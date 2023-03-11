#lang racket

(require "js-canvas-translator.rkt")
(require rackunit rackunit/text-ui)

(define-test-suite js-canvas-translator
  (test-case
   "variable definition"
   (check-equal? (scm->js (let ([v 42]) void))
                 "const v = 42;\n\n")
   (check-equal? (scm->js (let mut ([v s42]) void))
                 "let v = s42;\n\n")
   (check-equal? (scm->js (let ([a 1] [b 2] [c 3]) void))
                 "const a = 1;\nconst b = 2;\nconst c = 3;\n\n")
   (check-equal? (scm->js (let ([v 42]) void) (let mut ([w 43]) void))
                 "const v = 42;\n\nlet w = 43;\n\n"))
  (test-case
   "method call"
   (check-equal? (scm->js
                  (let ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "const elt = document.getElementById('fig-1');\n\n")
   (check-equal? (scm->js
                  (let mut ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "let elt = document.getElementById('fig-1');\n\n")))

(run-tests js-canvas-translator)