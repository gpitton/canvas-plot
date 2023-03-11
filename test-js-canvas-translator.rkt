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
   "object property"
   (check-equal? (scm->js (let mut ([a 1] [b 2]) (set! (b 'length) a)))
                 "let a = 1;\nlet b = 2;\nb.length = a\n\n")
   (check-equal? (scm->js (let mut ([b 1]) (set! a (b 'length-of))))
                 "let b = 1;\na = b.lengthOf\n\n"))
  (test-case
   "method call"
   (check-equal? (scm->js
                  (let ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "const elt = document.getElementById('fig-1');\n\n")
   (check-equal? (scm->js
                  (let mut ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "let elt = document.getElementById('fig-1');\n\n"))
  (test-case
   "assignment"
   (check-equal? (scm->js
                  (let mut ([x 42]) (set! x 3)))
                 "let x = 42;\nx = 3;\n\n")
   (check-equal? (scm->js
                  (let mut ([a 0] [b 1])
                    (set! a b)
                    (set! b ((document 'get-element-by-id) 'id))))
                 "let a = 0;\nlet b = 1;\na = b;\nb = document.getElementById('id');\n\n"))
  (test-case
   "lambda"
   (check-equal? (scm->js
                  (let ([f (λ (x) (set! x 5))]) void))
                 "function f(x) {\nx = 5;\n}\n\n")
   (check-equal? (scm->js
                  (let ([f (λ (x) (set! x 5) (set! x 6))]) void))
                 "function f(x) {\nx = 5;\nx = 6;\n}\n\n")))

(run-tests js-canvas-translator)