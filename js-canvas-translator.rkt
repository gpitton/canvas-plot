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
(define-for-syntax (to-syntax ctx str) (datum->syntax ctx str))

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


;; to-camel-case converts a string with a name in kebab-case form and returns
;; a string with the same name expressed in camel case form.
;; Examples:
;;   (to-camel-case "name-and-age") -> "nameAndAge"
;;   (to-camel-case 'name-and-age) -> "nameAndAge"
(define-for-syntax (to-camel-case x)
  (let* ([xs (cond [(string? x) x]
                   [(symbol? x) (symbol->string x)]
                   [else (error 'to-camel-case "unexpected input: ~a" x)])]
         [xs-chars (string->list xs)])
    (let-values
        ([(_ xs-camel-case-rev)
          (for/fold ([prev-was-hyphen #f]
                     [camel-case-list '()])
                    ([c xs-chars])
            (let ([c-is-hyphen (eq? c #\-)])
              (values c-is-hyphen
                      (cond [c-is-hyphen camel-case-list]
                            [prev-was-hyphen (cons (char-upcase c) camel-case-list)]
                            [else (cons c camel-case-list)]))))])
      (list->string (reverse xs-camel-case-rev)))))


;; document is a helper to encode the properties of the "document" object in
;; JavaScript. It accepts a method expressed as a symbol, and returns a string
;; encoding the JavaScript method or property that is encoded by the input symbol.
(define-for-syntax (document method)
  (case method
    ['get-element-by-id "getElementById"]))


;; scm->js:assign is a helper macro to process (that is, convert to JavaScript source) any bindings
;; that appear in a let or let mut form parsed by scm->js.
; TODO rename symbols using gensym
(define-syntax (scm->js:assign stx)
  (syntax-case stx (mut)
    [(_ ()) #'""]
    ;; Binding of a value to a symbol:
    ;; (let ([s v]) void) -> "const s = v;"
    [(_ [sym val])
     (and (stx-symbol? #'sym) (stx-atom? #'val))
     (with-syntax ([s (to-string #'sym)]
                   [v (to-string #'val)])
       #'(format "const ~a = ~a;" s v))]
    ;; Binding of a mutable value to a symbol:
    ;; (let mut ([s v]) void) -> "let s = v;"
    [(_ (mut [sym val]))
     (and (stx-symbol? #'sym) (stx-atom? #'val))
     (let ([s (to-string #'sym)]
           [v (to-string #'val)])
       (let ([source (format "let ~a = ~a;" s v)])
         (to-syntax #'sym source)))]
    ;; Binding the result of a unary method call to a symbol:
    ;; (let ([s ((obj 'method) arg)]) void)
    ;; -> "const s = obj.method(arg);"
    [(_ [sym ((obj method) arg)])
     (and (stx-symbol? #'sym) (stx-symbol? #'obj) (stx-quoted? #'method)
          (stx-atom? #'arg))
     (let ([s (to-string #'sym)]
           [obj-name (to-string #'obj)]
           [o (syntax->datum #'obj)]
           [m (syntax->datum #'method)]
           [a (normalise-argument #'arg)])
       (let ([source (format "const ~a = ~a.~a(~a);" s obj-name (eval `(,o ,m)) a)])
         (to-syntax #'sym source)))]
    [_ (error 'scm->js:assign "unexpected syntax: ~a" stx)]))


(define-syntax (scm->js stx)
  (syntax-case stx (let mut void)
    ;; Base cases for the recursion
    [(_ (let () void)) #'""]
    [(_ (let mut () void)) #'""]
    ;; General case: immutable bindings.
    [(_ (let (ex0 ex1 ...) void))
     #'(~a (scm->js:assign ex0) #\newline
           (scm->js (let (ex1 ...) void)))]
    ;; General case: mutable bindings.
    [(_ (let mut (ex0 ex1 ...) void))
     #'(~a (scm->js:assign (mut ex0)) #\newline
           (scm->js (let mut (ex1 ...) void)))]
    [_ (error 'scm->js "unexpected syntax: ~a" stx)]))
