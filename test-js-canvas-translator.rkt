#lang racket

(require "js-canvas-translator.rkt")
(require rackunit rackunit/text-ui)

(define-test-suite js-canvas-translator
  (test-case
   "empty let bindings"
   (check-equal? (scm->js (let () void)) "")
   (check-equal? (scm->js (let mut () void)) ""))
  (test-case
   "variable definition"
   (check-equal? (scm->js (let ([v 42]) void))
                 "const v = 42;\n")
   (check-equal? (scm->js (let mut ([v s42]) void))
                 "let v = s42;\n")
   (check-equal? (scm->js (let ([a 1] [b 2] [c 3]) void))
                 "const a = 1;\nconst b = 2;\nconst c = 3;\n")
   (check-equal? (scm->js (let ([v 42]) void) (let mut ([w 43]) void))
                 "const v = 42;\nlet w = 43;\n")
   ;; Test with missing "void" keyword.
   (check-equal? (scm->js (let mut ([a 5] [b a] [d "hello"])))
                 "let a = 5;\nlet b = a;\nlet d = 'hello';\n"))
  (test-case
   "object property"
   (check-equal? (scm->js (let mut ([a 1] [b 2]) (set! (b 'length) a)))
                 "let a = 1;\nlet b = 2;\nb.length = a;\n")
   (check-equal? (scm->js (let mut ([b 1]) (set! a (b 'length-of))))
                 "let b = 1;\na = b.lengthOf;\n"))
  (test-case
   "method call"
   (check-equal? (scm->js
                  (let ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "const elt = document.getElementById('fig-1');\n")
   (check-equal? (scm->js
                  (let mut ([elt ((document 'get-element-by-id) 'fig-1)])
                    void))
                 "let elt = document.getElementById('fig-1');\n"))
  (test-case
   "assignment"
   (check-equal? (scm->js
                  (let mut ([x 42]) (set! x 3)))
                 "let x = 42;\nx = 3;\n")
   (check-equal? (scm->js
                  (let mut ([a 0] [b 1])
                    (set! a b)
                    (set! b ((document 'get-element-by-id) 'id))))
                 "let a = 0;\nlet b = 1;\na = b;\nb = document.getElementById('id');\n")
   (check-equal? (scm->js (let mut ([x 1]) (set! x 2)
                            (set! x (ctx 'property)) (set! x ((obj ' method) 0 2 x))))
                 "let x = 1;\nx = 2;\nx = ctx.property;\nx = obj.method(0, 2, x);\n"))
  (test-case
   "arithmetic expression"
   (check-equal? (scm->js (let ([x (+ (* 1 2) (/ 3 (- 5 6)))]) void))
                 "const x = ( 1 * 2) + (3 / (5 - 6) ) ;\n"))
  (test-case
   "conditional statements"
   (check-equal? (scm->js (let ([s 1] [t #t]) (if t (begin (set! s y)))))
                 "const s = 1;\nconst t = true;\nif (t) {\ns = y;\n}\n"))
  (test-case
   "function call"
   (check-equal? (scm->js (let () (f)))
                 "f();\n")
   (check-equal? (scm->js (let () (f a b c)))
                 "f(a, b, c);\n")
   (check-equal? (scm->js (let () (some-fun-name a4 a3 a2 a-last)))
                 "someFunName(a4, a3, a2, aLast);\n"))
  (test-case
   "lambda"
   (check-equal? (scm->js
                  (let ([f (λ (x) (begin (set! x 5)))]) void))
                 "function f(x) {\n{\nx = 5;\n}\n}\n")
   (check-equal? (scm->js
                  (let ([f (λ (x) (begin (set! x 5) (set! x 6)))]) void))
                 "function f(x) {\n{\nx = 5;\nx = 6;\n}\n}\n")
   (check-equal? (scm->js
                  (let ([f (λ (x) (set! x 5) (set! x 6))]) void))
                 "function f(x) {\nx = 5;\nx = 6;\n}\n")
   (check-equal? (scm->js
                  (let ([f (λ (x) (let ([y 3]) (set! x 5) (set! x y)))]) void))
                 "function f(x) {\nconst y = 3;\nx = 5;\nx = y;\n}\n"))
  (test-case
   "for loop"
   (check-equal? (scm->js
                  (let () (for (i (in-range 0 2 incr)) (set! y i))))
                 "for (let i = 0; i < 2; ++i) {\ny = i;\n}\n")
   (check-equal? (scm->js
                  (let ([h 5]) (for (i (in-range 0 (+ h 3) decr)) (set! y i))))
                 "const h = 5;\nfor (let i = 0; i < h + 3; --i) {\ny = i;\n}\n")
   (check-equal? (scm->js (let () (for (i (in-range 0 2 incr)) (let () void))))
                 "for (let i = 0; i < 2; ++i) {\n}\n")
   (check-equal? (scm->js (let () (for (i (in-range 0 2 incr)) (let mut ([x 3]) void))))
                 "for (let i = 0; i < 2; ++i) {\nlet x = 3;\n}\n")
   (check-equal? (scm->js (let () (for (i (in-range 0 2 incr)) (let ([x 3]) (set! x i)))))
                 "for (let i = 0; i < 2; ++i) {\nconst x = 3;\nx = i;\n}\n")
   (check-equal? (scm->js
                  (let ([h 5]) (for (i (in-range 0 (+ h 3) decr)) (let ([b 2]) (set! y (+ i b))))))
                 "const h = 5;\nfor (let i = 0; i < h + 3; --i) {\nconst b = 2;\ny = i + b;\n}\n"))
  (test-case
   "for and lambda"
   (check-equal? (scm->js (let ([a (λ () (let () (for (i (in-range 0 1 incr)) (set! x i))))])))
                 "function a() {\nfor (let i = 0; i < 1; ++i) {\nx = i;\n}\n}\n")
   (check-equal? (scm->js (let ([a (λ () (for (i (in-range 0 1 incr)) (set! x i)))])))
                 "function a() {\nfor (let i = 0; i < 1; ++i) {\nx = i;\n}\n}\n")))

(run-tests js-canvas-translator)