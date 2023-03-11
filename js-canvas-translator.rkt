#lang racket

(provide (for-syntax document) scm->js)

;; Example of a scheme-like syntax to write canvas helper functions
;; that compile to JavaScript.
#|
(let* ([elt ((document 'get-element-by-id) 'fig-1)]
       [ctx ((elt 'get-context) '2d)]
       [w (elt 'client-width)]
       [h (elt 'client-height)])
  (set! (elt 'width) w)
  (set! (elt 'height) h))

;; should translate to:
"
var gensym0 = document.getElementById('fig-1');
var gensym1 = gensym0.getContext('2d');
var gensym2 = gensym0.clientWidth;
var gensym3 = gensym0.clientHeight;
gensym0.width = gensym2;
gensym0.height = gensym3;
"
|#


(define-for-syntax (stx-number? stx) (number? (syntax->datum stx)))
(define-for-syntax (stx-symbol? stx) (symbol? (syntax->datum stx)))
(define-for-syntax (stx-string? stx) (string? (syntax->datum stx)))

;; (stx-quoted? 'x) -> #t
;; (stx-quoted? (quote x)) -> #t
;; (stx-quoted? (list 'x 'y)) -> #f
(define-for-syntax (stx-quoted? stx)
  (let ([arg (syntax->datum stx)])
    (and (list? arg)
         (not (null? arg))
         (eq? (car arg) 'quote)
         (not (null? (cdr arg)))
         (null? (cddr arg)))))

(define-for-syntax (stx-atom? stx) (or (stx-number? stx) (stx-symbol? stx) (stx-quoted? stx)))

(define-for-syntax (to-string stx) (format "~a" (syntax->datum stx)))
(define-for-syntax (to-syntax pos str) (datum->syntax pos str))

;; normalise-argument takes a syntax object and returns a string that encodes
;; the appropriate javascript object which is supposed to appear as a function
;; argument. Examples:
;;   (normalise-argument 42) -> "42"
;;   (normalise-argument 'id) -> "'id'"
;;   (normalise-argument "id") -> "'id'"
(define-for-syntax (normalise-argument stx)
  (let ([arg (syntax->datum stx)])
    (cond [(null? arg) ""]
          [(stx-number? stx) (format "~a" arg)]
          [(stx-quoted? stx) (format "'~a'" (cadr arg))]
          [(string? arg) (format "'~a'" arg)]
          [else (error 'normalise-argument "unsupported format for ~a." arg)])))


;; A helper to encode the properties of the "document" object in javascript.
;; It accepts a method expressed as a symbol, and returns a string encoding
;; the JavaScript method or property that is encoded by the input symbol.
(define-for-syntax (document method)
  (case method
    ['get-element-by-id "getElementById"]))


; TODO rename symbols using gensym
(define-syntax (scm->js stx)
  (syntax-case stx (let mut void)
    [(_ (let () void)) #'""]
    ;; Binding of a value to a symbol:
    ;; (let ([s v]) void) -> "const s = v;"
    [(_ (let ([sym val]) void))
     (and (stx-symbol? #'sym) (stx-atom? #'val))
     (with-syntax ([s (to-string #'sym)]
                   [v (to-string #'val)])
       #'(format "const ~a = ~a;" s v))]
    ;; Binding of a mutable value to a symbol:
    ;; (let mut ([s v]) void) -> "let s = v;"
    [(_ (let mut ([sym val]) void))
     (and (stx-symbol? #'sym) (stx-atom? #'val))
     (let ([s (to-string #'sym)]
           [v (to-string #'val)])
       (let ([source (format "let ~a = ~a;" s v)])
         (to-syntax #'sym source)))]
    ;; Binding the result of a unary method call to a symbol:
    ;; (let ([s ((obj 'method) arg)]) void)
    ;; -> "const s = obj.method(arg);"
    [(_ (let ([sym ((obj method) arg)]) void))
     (and (stx-symbol? #'sym) (stx-symbol? #'obj) (stx-quoted? #'method)
          (stx-atom? #'arg))
     (let ([s (to-string #'sym)]
           [obj-name (to-string #'obj)]
           [o (syntax->datum #'obj)]
           [m (syntax->datum #'method)]
           [a (normalise-argument #'arg)])
       (let ([source (format "const ~a = ~a.~a(~a);" s obj-name (eval `(,o ,m)) a)])
         (to-syntax #'sym source)))]
    [_ #'"unexpected syntax"]))
