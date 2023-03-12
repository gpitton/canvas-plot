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
                    ([c (in-list xs-chars)])
            (let ([c-is-hyphen (eq? c #\-)])
              (values c-is-hyphen
                      (cond [c-is-hyphen camel-case-list]
                            [prev-was-hyphen (cons (char-upcase c) camel-case-list)]
                            [else (cons c camel-case-list)]))))])
      (list->string (reverse xs-camel-case-rev)))))


;; bind-qualifier has two possibly inputs: a syntax object whose content is either
;; const or mut and converts them respectively to the strings "const" and "let".
(define-for-syntax (bind-qualifier qual)
  (let ([q (syntax->datum qual)])
    (cond [(eq? q 'const) "const"]
          [(eq? q 'mut) "let"]
          [else (error 'bind-qualifier "bad input: ~a" qual)])))


;; get-property translates to JavaScript the access of a property of an object.
(define-for-syntax (get-property obj property)
  (let* ([obj-symbol (syntax->datum obj)]
         [prop-symbol (syntax->datum property)]
         [prop-name (to-camel-case (cadr prop-symbol))])
    (format "~a.~a" obj-symbol prop-name)))


;; method-call translates to JavaScript the call of a unary method of obj with
;; argument arg.
(define-for-syntax (method-call obj method arg)
  (let ([obj-name (to-string obj)]
        [o (syntax->datum obj)]
        [m (syntax->datum method)]
        [a (normalise-argument arg)])
    (format "~a.~a(~a)" obj-name (eval `(,o ,m)) a)))


;; document is a helper to encode the properties of the "document" object in
;; JavaScript. It accepts a method expressed as a symbol, and returns a string
;; encoding the JavaScript method or property that is encoded by the input symbol.
(define-for-syntax (document method)
  (case method
    ['get-element-by-id "getElementById"]))


;; scm->js:lambda is a helper macro to process (i.e. convert to JavaScript source)
;; the body of a function definition from a let or let mut binding parsed by
;; scm->js:declare.
(define-syntax (scm->js:lambda stx)
  (syntax-case stx (let mut)
    [(_) #'"}\n"]  ;; Nothing to do. Close lambda scope.
    ;; Local declarations. Forward to scm->js.
    [(_ (let (ex ...) body ...))
     #'(scm->js (let (ex ...) body ...))]
    [(_ (let mut (ex ...) body ...))
     #'(scm->js (let mut (ex ...) body ...))]
    ;; Local assignment. Forward to scm->js:assign.
    [(_ ex0 ex1 ...)
     #'(~a (scm->js:assign ex0)
           (scm->js:lambda ex1 ...))]
    [_ (error 'scm->js:lambda "unexpected syntax ~a" stx)]))


;; scm->js:declare is a helper macro to process (i.e. convert to JavaScript source)
;; any bindings that appear in a let or let mut form parsed by scm->js.
; TODO rename symbols using gensym
(define-syntax (scm->js:declare stx)
  (syntax-case stx (λ)
    ;[(_ (let ())) #'""]
    ;; Binding of a value to a symbol. qual is the const/mut qualifier.
    ;; Examples:
    ;;   (([s v]) void) -> "const s = v;"
    ;;   (mut ([s v]) void) -> "let s = v;"
    [(_ (qual [sym val]))
     ;; TODO check that qual is const or mut.
     (and (stx-symbol? #'sym) (stx-atom? #'val))
     (let ([q (bind-qualifier #'qual)]
           [s (to-string #'sym)]
           [v (to-string #'val)])
       (let ([source (format "~a ~a = ~a;\n" q s v)])
         (to-syntax #'sym source)))]
    ;; Binding the result of a unary method call to a symbol. qual is the const/mut qualifier.
    ;; Example:
    ;;   (([s ((obj 'method) arg)]) void)
    ;;   -> "const s = obj.method(arg);"
    [(_ (qual [sym ((obj method) arg)]))
     ;; TODO check that qual is const or mut.
     (and (stx-symbol? #'sym) (stx-symbol? #'obj) (stx-quoted? #'method)
          (stx-atom? #'arg))
     (let ([q (bind-qualifier #'qual)]
           [s (to-string #'sym)]
           [fun (method-call #'obj #'method #'arg)])
       (let ([source (format "~a ~a = ~a;\n" q s fun)])
         (to-syntax #'sym source)))]
    ;; Bind a function declaration (lambda) to a name.
    ;; Example:
    ;;  (([f (λ (x) (let (...) ...))]) void)
    ;;  -> function f(x) { ... }
    ;; The const/mut qualifier is going to be discarded for now.
    [(_ (qual [sym (λ (arg) body ...)]))
     (and (stx-symbol? #'sym) (stx-symbol? #'arg))
     (with-syntax ([source (format "function ~a(~a) {\n"
                                   (to-string #'sym) (to-string #'arg))])
       #'(string-append source
                        (scm->js:lambda body ...)))]
    [_ (error 'scm->js:declare "unexpected syntax: ~a" stx)]))


;; scm->js:assign is a helper macro to process (that is, convert to JavaScript
;; source) any assignment operators (using set!) that appear in the body of a
;; let or let mut form parsed by scm->js.
; TODO rename symbols using gensym
(define-syntax (scm->js:assign stx)
  (syntax-case stx (set!)
    ;[(_ (let ())) #'""]
    ;; Assign a new value to an existing symbol.
    ;; Example:
    ;;   (set! s v) -> "s = v;"
    [(_ (set! sym val))
     (and (stx-symbol? #'sym) (stx-atom? #'val))
     (let ([s (to-string #'sym)]
           [v (to-string #'val)])
       (let ([source (format "~a = ~a;\n" s v)])
         (to-syntax #'sym source)))]
    ;; Assign an object property to an existing symbol.
    ;; Example:
    ;;   (set! symbol (obj 'property)) -> "symbol = obj.property;"
    [(_ (set! sym (obj property)))
     (and (stx-symbol? #'sym) (stx-symbol? #'obj) (stx-quoted? #'property))
     (let* ([lhs (to-string #'sym)]
            [rhs (get-property #'obj #'property)]
            [source (format "~a = ~a\n" lhs rhs)])
       (to-syntax #'sym source))]
    ;; Assign a value to an object property.
    ;; Example:
    ;;   (set! (obj 'property) value) -> "obj.property = value;"
    [(_ (set! (obj property) value))
     (and (stx-symbol? #'obj) (stx-quoted? #'property))
     (let* ([lhs (get-property #'obj #'property)]
            [rhs (to-string #'value)]
            [source (format "~a = ~a\n" lhs rhs)])
       (to-syntax #'sym source))]
    ;; Assign the result of a unary method call to an existing symbol.
    ;; Example:
    ;;   (set! s ((obj 'method) arg)) -> "s = obj.method(arg);"
    [(_ (set! sym ((obj method) arg)))
     (and (stx-symbol? #'sym) (stx-symbol? #'obj) (stx-quoted? #'method)
          (stx-atom? #'arg))
     (let ([s (to-string #'sym)]
           [fun (method-call #'obj #'method #'arg)])
       (let ([source (format "~a = ~a;\n" s fun)])
         (to-syntax #'sym source)))]
    [_ (error 'scm->js:assign "unexpected syntax: ~a" stx)]))


;; Main driver for the scheme to JavaScript translator.
;; Handles a block of expressions introduced by a let statement.
;; Example usage:
;; (scm->js
;;  (let ([a 1])
;;    void)
;;  (let mut ([b 4])
;;    (set! b 5)))
; TODO add { and }
(define-syntax (scm->js stx)
  (syntax-case stx (let mut void)
    ;; Recursion complete.
    [(_) #'""]
    ;; Current block complete. Close the current scope and move on to
    ;; the next one.
    [(_ (() void) block ...)
     #'(~a "}\n\n" (scm->js block ...))]
    ;; Current mutable block complete. Close the current scope and move on to
    ;; the next one.
    [(_ (mut () void) block ...)
     #'(~a "}\n\n" (scm->js block ...))]
    ;; Mutable let bindings complete: process the last element of the current block.
    ;; This is the base case for the next syntax-case block.
    [(_ (mut () ex) block ...)
     #'(~a (scm->js:assign ex)
           ;; We need to put a void at the end to signal that we completed parsing
           ;; the current block.
           (scm->js (() void))
           (scm->js block ...))]
    ;; Immutable let bindings complete: process the last element of the current block.
    ;; This is the base case for the next syntax-case block.
    [(_ (() ex) block ...)
     ; TODO assignment not allowed here.
     #'(~a (scm->js:assign ex)
           ;; We need to put a void at the end to signal that we completed parsing
           ;; the current block.
           (scm->js (() void))
           (scm->js block ...))]
    ;; Process the body (mutable case).
    [(_ (mut () ex0 ex1 ...) block ...)
     #'(~a (scm->js:assign ex0)
           (scm->js (mut () ex1 ...) block ...))]
    ;; Immutable bindings complete: process the last element of the current block.
    ;; This is the base case for the next syntax-case block.
    [(_ (() ex) block ...)
     ; TODO assignment is not allowed here.
     #'(~a (scm->js:assign ex)
           ;; We need to put a void at the end to signal that we completed parsing
           ;; the current block.
           (scm->js (() void))
           (scm->js block ...))]
    ;; Let bindings complete: now process the body.
    [(_ (() ex0 ex1 ...) block ...)
     ; TODO assignment is not allowed here.
     #'(~a (scm->js:assign ex0)
           (scm->js (() ex1 ...) block ...))]
    ;; General case: mutable bindings. Order of pattern match is important, and
    ;; the mutable case must precede the immutable case.
    [(_ (mut (ex0 ex1 ...) body ...) block ...)
     #'(~a (scm->js:declare (mut ex0))
           (scm->js (mut (ex1 ...) body ...) block ...))]
    ;; General case: immutable bindings.
    [(_ ((ex0 ex1 ...) body ...) block ...)
     #'(~a (scm->js:declare (const ex0))
           (scm->js ((ex1 ...) body ...) block ...))]
    ;; Initialise the recursion (mutable case). We need to open a new scope.
    [(_ (let mut (ex ...) body ...) block ...)
     #'(~a "{\n" (scm->js (mut (ex ...) body ...) block ...))]
    ;; Initialise the recursion (immutable case). We need to open a new scope.
    [(_ (let (ex ...) body ...) block ...)
     #'(~a "{\n" (scm->js ((ex ...) body ...) block ...))]
    [_ (error 'scm->js "unexpected syntax: ~a" stx)]))

