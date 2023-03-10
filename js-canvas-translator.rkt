#lang racket


;; Example of a scheme-like syntax to write canvas helper functions
;; that compile to JavaScript.
#|
(let* ([elt ((document 'get-element-by-id) 'fig-1)]
       [ctx ((elt 'get-context) '2d)]
       [w (elt 'client-width)]
       [h (elt 'client-height)])
  (set! (elt 'width) w)
  (set! (elt 'height) h))
|#
;; should translate to:
"
var gensym0 = document.getElementById('fig-1');
var gensym1 = gensym0.getContext('2d');
var gensym2 = gensym0.clientWidth;
var gensym3 = gensym0.clientHeight;
gensym0.width = gensym2;
gensym0.height = gensym3;
"

;; example ---------------------------------------------------------------------
;(let ([elt ((document 'get-element-by-id) 'fig-1)]) void)
;; translates to:
"
var gensym0 = document.getElementById('fig-1');
"

(define-syntax (scm->js stx)
  (syntax-case stx (let void)
    [(_ (let () void)) #'""]
    [(_ (let ([sym0 (ex1 sym1)]) void))
     ;(and (symbol? sym0) (symbol? sym1))
     ; TODO rename sym0 using gensym
     (with-syntax ([s0 (format "~a" (syntax->datum #'sym0))]
                   [s1 (format "~a" (syntax->datum #'sym1))])
       #'(format "let ~a = ~a;" s0 s1))]
    [_ #'"unexpected syntax"]))

;; TODO put scm->js in a module or the example here will not work
;(scm->js (let ([elt ((document 'get-element-by-id) 'fig-1)]) void))
(displayln (scm->js (let ([v (s n)]) void)))