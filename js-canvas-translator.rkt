#lang racket

;; Done:
;; - command,
;; - definition-command,
;; - bind,
;; - lambda,
;; - expr,
;; - object-method,
;; - object-property
;; Missing:
;; - conditional statements,
;; - conditional expressions,
;; - arithmetic expression
;; - comment everything
;; - test properly

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

(define-for-syntax (stx-boolean? stx) (boolean? (syntax->datum stx)))
(define-for-syntax (stx-number? stx) (number? (syntax->datum stx)))
(define-for-syntax (stx-symbol? stx) (symbol? (syntax->datum stx)))
(define-for-syntax (stx-string? stx) (string? (syntax->datum stx)))


;; quoted? returns true if its argument is a list of two elements, and the
;; first element is 'quote.
(define-for-syntax (quoted? arg)
  (and (list? arg)
       (not (null? arg))
       (eq? (car arg) 'quote)
       (not (null? (cdr arg)))
       (null? (cddr arg))))


;; (stx-quoted? 'x) -> #t
;; (stx-quoted? (quote x)) -> #t
;; (stx-quoted? (list 'x 'y)) -> #f
(define-for-syntax (stx-quoted? stx)
  (let ([arg (syntax->datum stx)])
    (quoted? arg)))

(define-for-syntax (stx-atom? stx) (or (stx-number? stx) (stx-symbol? stx) (stx-quoted? stx)))

(define-for-syntax (to-string stx) (format "~a" (syntax->datum stx)))

(define-for-syntax (to-syntax ctx str) (datum->syntax ctx str))


;; normalise-arguments takes a syntax object and returns a string that encodes
;; the appropriate javascript object which is supposed to appear as a function
;; argument. Examples:
;;   (normalise-arguments '(42)) -> "42"
;;   (normalise-arguments '(id)) -> "'id'"
;;   (normalise-arguments '('id)) -> "'id'"
;;   (normalise-arguments '("id", tag, 42)) -> "'id', 'tag', 42"
(define-for-syntax (normalise-arguments args)
  (define (argument-handler x)
    (cond [(null? x) ""]
          [(number? x) (format "~a" x)]
          [(quoted? x) (format "'~a'" (cadr x))]
          [(symbol? x) (format "~a" x)]
          [(string? x) (format "'~a'" x)]
          [else (error 'normalise-arguments "unsupported format for ~a." x)]))
  (let* (;; If the function is called with a single argument, then args is a list
         ;; with just one member, for example: args = ('id).
         ;; If the function is called with more than one arguments, then args is
         ;; a list with a list containing the arguments, for example:
         ;;   args = (('foo 42 "bar"))
         [args-list (if (and (list? (car args))
                             (not (quoted? (car args))))
                        (car args)
                        args)])
    (foldl (λ (arg acc)
             (if (string=? acc "")
                 (argument-handler arg)
                 (string-append acc ", " (argument-handler arg))))
           ""
           args-list)))


;; to-camel-case converts a string with a name in kebab-case form and returns
;; a string with the same name expressed in camel case form.
;; Examples:
;;   (to-camel-case "name-and-age") -> "nameAndAge"
;;   (to-camel-case 'name-and-age) -> "nameAndAge"
(define-for-syntax (to-camel-case x)
  (let* ([xs (cond [(string? x) x]
                   [(symbol? x) (symbol->string x)]
                   [(quoted? x) (symbol->string (cadr x))]
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


;; A syntax object is a qualifier if it is one of 'const or 'mut.
(define-for-syntax (stx-qualifier? stx)
  (let ([qual (syntax->datum stx)])
    (member qual '(const mut))))


;; bind-qualifier has two possibly inputs: a syntax object whose content is either
;; const or mut and converts them respectively to the strings "const" and "let".
(define-for-syntax (bind-qualifier qual)
  (let ([q (syntax->datum qual)])
    (cond [(eq? q 'const) "const"]
          [(eq? q 'mut) "let"]
          [else (error 'bind-qualifier "bad input: ~a" qual)])))


;; stx-arithmetic? returns true if the argument is one of the supported arithmetic
;; operators.
(define-for-syntax (stx-arithmetic? stx)
  (let ([op (syntax->datum stx)])
    (member op '(+ - * /))))


;; object-property translates to JavaScript the access of a property of an object.
(define-syntax (object-property stx)
  (syntax-case stx ()
    [(_ obj property)
     (and (stx-symbol? #'obj) (stx-quoted? #'property))
     (let* ([obj-str (to-string #'obj)]
            [prop-symbol (syntax->datum #'property)]
            [prop-str (to-camel-case (cadr prop-symbol))]
            [source (format "~a.~a" obj-str prop-str)])
       (datum->syntax #'stx source))]
    [_ (error 'object-property "unexpected syntax: ~a" stx)]))


;; document is a helper to encode the properties of the "document" object in
;; JavaScript. It accepts a method expressed as a symbol, and returns a string
;; encoding the JavaScript method or property that is encoded by the input symbol.
(define-for-syntax (document method)
  (case method
    ['get-element-by-id "getElementById"]))


;; known-canvas-objects is a list of the canvas objects that we support.
(define-for-syntax known-canvas-objects '(document))


;; method-call translates to JavaScript the call of a method of obj with
;; argument arg.
(define-syntax (method-call stx)
  (syntax-case stx ()
    [(_ obj method arg ...)
     (and (stx-symbol? #'obj) (stx-symbol? #'method))
     (let* ([obj-str (to-string #'obj)]
            [obj-d (syntax->datum #'obj)]
            [method-d (syntax->datum #'method)]
            [arg-d (syntax->datum #'(arg ...))]
            [arg-str (if (null? arg-d) "" (normalise-arguments arg-d))]
            [member-name
             (if (member obj-d known-canvas-objects)
                 (eval `(,obj-d ,method-d))
                 (to-camel-case method-d))]
            [source (format "~a.~a(~a)" obj-str member-name arg-str)])
       (datum->syntax #'stx source))]
    [_ (error 'method-call "unexpected syntax: ~a" stx)]))

;; Below cdsl stands for: canvas-DSL.

;; expression:
;;     value
;;    | symbol
;;    | object-property
;;    | lambda-call expression+
;;    | (if expression expression)
;;    | (if expression expression expression)
(define-syntax (cdsl:expr stx)
  (syntax-case stx ()
    ;; Base case for the expansion of (cdsl:expr ex ...)
    [(_) #'""]
    ;; Boolean value.
    [(_ v)
     (stx-boolean? #'v)
     (datum->syntax #'v (if (syntax->datum #'v) "true" "false"))]
    ;; Quoted symbol: 'id -> "'id'" useful for transformations like:
    ;; ((obj 'method) 'id) -> "obj.method('id')"
    [(_ v)
     (stx-quoted? #'v)
     (let ([v-str (normalise-arguments (list (syntax->datum #'v)))])
       (datum->syntax #'v v-str))]
    [(_ v)
     (or (stx-number? #'v) (stx-symbol? #'v) (stx-string? #'v))
     (datum->syntax #'v (to-string #'v))]
    ;; Access to an object's property.
    [(_ (obj prop))
     (and (stx-symbol? #'obj) (stx-quoted? #'prop))
     #'(object-property obj prop)]
    ;; Lambda call.
    [(_ (op ex ...))
     (stx-symbol? #'op)
     (let ([op-str (format "~a(" (to-string #'op))])
       #`(~a #,op-str (cdsl:expr ex ...) ");\n"))]
    ;; Call an object's method.
    ; TODO this requires a change of the grammar.
    [(_ ((obj method) ex0 ex1 ...))
     (and (stx-symbol? #'obj) (stx-quoted? #'method))
     ;; Generate the string encoding an object's method call with a workaround
     ;; involving object-property, due to being unable to pass cdsl:expr as an
     ;; argument to cdsl:method-call:
     ;; (cdsl:method-call obj prop (cdsl:expr #'(ex ...))) does not work.
     #'(string-append (object-property obj method) "("
                      (string-join (list (cdsl:expr ex0) (cdsl:expr ex1) ...) ", ")
                      ")")]
    [_ (error 'cdsl:expr "unexpected syntax: ~a" stx)]))


;; cdsl:lambda is a helper macro to process (convert to JavaScript source)
;; the body of a function definition.
;;
;; lambda: (λ (symbol+) definition-command+)
(define-syntax (cdsl:lambda stx)
  (syntax-case stx (begin let mut)
    ;; Nothing to do. Close the lambda's scope.
    [(_) #'"}\n"]
    ;; Parsing of the function body is complete. Close the lambda's scope.
    [(_ (begin)) #'"}\n"]
    ;; Local declarations. Forward to scm->js.
    [(_ (let (ex ...) body ...))
     #'(string-append (scm->js (let (ex ...) body ...)) "}\n")]
    ;; Local declarations, mutable version. Forward to scm->js.
    [(_ (let mut (ex ...) body ...))
     #'(string-append (scm->js (let mut (ex ...) body ...)) "}\n")]
    ;; Start parsing the lambda's body. Open a scope for the lambda, then forward
    ;; each expression to cdsl:command.
    [(_ (begin ex0 ex1 ...))
     #'(~a "{\n"
           (cdsl:command ex0)
           (cdsl:lambda ex1 ...)
           "}\n")]
    ;; Case of a sequence of statements with just one statement.
    [(_ (begin ex0))
     #'(string-append (cdsl:command ex0) (cdsl:lambda))]
    ;; Keep parsing the lambda's body (recursive case). Just forward each expression
    ;; to scm->js and recur.
    [(_ ex0 ex1 ...)
     #'(~a (scm->js (let () ex0))
           (cdsl:lambda ex1 ...))]
    ;; Keep parsing the lambda's body (recursive case). Just one definition-command left.
    [(_ ex0)
     #'(string-append (scm->js (let () ex0)) (cdsl:lambda))]
    [_ (error 'cdsl:lambda "unexpected syntax ~a" stx)]))


;; binding:
;;    []
;;    | [symbol expression]
;;    | [symbol lambda-definition]
(define-syntax (cdsl:bind stx)
  (syntax-case stx (λ)
    ;; Base case for the recursion on bind ...
    [(_ qual) (stx-qualifier? #'qual) #'""]
    [(_ qual [sym (λ (id ...) body ...)] bind ...)
     (stx-symbol? #'sym)
     ;; Here we do not need to bind the qualifier: we assume function bindings
     ;; cannot be redefined.
     (let* ([sym-str (to-string #'sym)]
            [args (syntax->datum #'(id ...))]
            [args-str (if (null? args) "" (normalise-arguments args))]
            [source (format "function ~a(~a) {\n" sym-str args-str)])
       #`(string-append #,source (cdsl:lambda body ...)
                        (cdsl:bind qual bind ...)))]
    ;; If it's not a lambda definition, then it must be an expression.
    [(_ qual [sym ex] bind ...)
     (stx-symbol? #'sym)
     (let* ([qual-str (bind-qualifier #'qual)]
            [sym-str (to-string #'sym)]
            [bind-lhs (format "~a ~a = " qual-str sym-str)])
       #`(string-append #,bind-lhs
                        (cdsl:expr ex) ";\n"
                        (cdsl:bind qual bind ...)))]
    [_ (error 'cdsl:bind "unexpected syntax: ~a" stx)]))


;; command:
;;    (begin definition-command+)
;;    | assignment definition-command+
;;    ;; evaluate a lambda for side-effects
;;    | lambda-call definition-command+
;;    ;; conditional execution for side-effects (also below)
;;    | (if expression definition-command)
;;    | (if expression definition-command definition-command)
;;    | (if boolean-or-symbol definition-command)
;;    | (if boolean-or-symbol definition-command definition-command)
(define-syntax (cdsl:command stx)
  (syntax-case stx (begin if let set!)
    [(_) #'""]
    ;; Open a new scope with a leading let statement.
    [(_ (let ex ...) cmd ...)
     #'(string-append "{\n" (scm->js ex ...) (cdsl:command cmd ...) "}\n")]
    ;; Open a new scope, with a leading begin (for side-effects).
    [(_ (begin ex ...) cmd ...)
     #'(string-append "{\n" (cdsl:command ex ...) (cdsl:command cmd ...) "}\n")]
    ;; Assignment to a symbol.
    [(_ (set! sym ex) cmd ...)
     (stx-symbol? #'sym)
     (let ([sym-str (to-string #'sym)])
       #`(string-append #,sym-str " = "
                        (cdsl:expr ex) ";\n"
                        (cdsl:command cmd ...)))]
    ;; Assignment to a method property.
    [(_ (set! (obj prop) ex) cmd ...)
     (and (stx-symbol? #'obj) (stx-quoted? #'prop))
     (let ([sym-str (to-string #'sym)])
       #`(string-append (object-property obj prop) " = "
                        (cdsl:expr ex) ";\n"
                        (cdsl:command cmd ...)))]
    ;; Binary if statement (with variable test).
    [(_ (if test-ex (cmd ...)))
     (or (stx-boolean? #'test-ex) (stx-symbol? #'test-ex))
     (let ([test-str (to-string #'test-ex)])
     #`(string-append "if (" #,test-str ") "
                      (cdsl:command (cmd ...))))]
    ;; Binary if statement (with expression test)
    [(_ (if (test-ex) (cmd ...)))
     #'(string-append "if (" (cdsl:expr test-ex) ") "
                      (cdsl:command (cmd ...)))]
    ;; Ternary if statement (with variable test).
    ; TODO
    ;; Ternary if statement (with expression test).
    ; TODO
    ;; Function call for its side effects.
    [(_ (op ex ...) cmd ...)
     (stx-symbol? #'op)
     (let ([op-str (format "~a(" (to-string #'op))])
       #`(~a #,op-str (cdsl:expr ex ...) ");\n"
             (cdsl:command cmd ...)))]
    ;; Method call for its side effects.
    [(_ ((obj method) ex ...) cmd ...)
     (and (stx-symbol? #'obj) (stx-symbol? #'method))
     #'(method-call obj method ex ...)]
    ;[(_ ex0 ex1 ...) #'(cdsl:command ex1 ...)]))
    [_ (error 'cdsl:command "unexpected syntax: ~a" stx)]))


;; Main driver for the scheme to JavaScript translator.
;; program:
;;   definition-command+
;; definition-command:
;;   (let (binding+) void)
;;   (let (binding+) command+)
;;   (let mut (binding+) void)
;;   (let mut (binding+) command+)
(define-syntax (scm->js stx)
  (syntax-case stx (let mut void)
    ;; Program complete.
    [(_) #'""]
    ;; Current let binding complete.
    [(_ (let (ex ...) void) def-cmd ...)
     #'(string-append (cdsl:bind const ex ...) (scm->js def-cmd ...))]
    [(_ (let mut (ex ...) void) def-cmd ...)
     #'(string-append (cdsl:bind mut ex ...) (scm->js def-cmd ...))]
    ;; General case.
    [(_ (let (ex ...) cmd ...) def-cmd ...)
     #'(string-append (cdsl:bind const ex ...) (cdsl:command cmd ...)
                      (scm->js def-cmd ...))]
    ;; General case (mutable).
    [(_ (let mut (ex ...) cmd ...) def-cmd ...)
     #'(string-append (cdsl:bind mut ex ...) (cdsl:command cmd ...)
                      (scm->js def-cmd ...))]
    [_ (error 'scm->js "unexpected syntax: ~a" stx)]))
