#lang racket

(provide js:make-request)
(provide js:fetch)
(provide javascript)


(define (scheme-str->js-str fmt)
    (string-replace fmt "~a" "${~a}"))

(define (js:make-request #:host [host "localhost"] #:port port)
    (let ([hdr-str (symbol->string (gensym))]
          [req-str (symbol->string (gensym))]
          [code-str (format "const hdr = new Headers();\nconst req = new Request('http://~a:~a',\n\
    {method: 'GET', action: '/', headers: hdr});\n" host port)])
         (values
           (string-replace
             (string-replace code-str "hdr" hdr-str)
             "req" req-str)
          req-str)))

(define (js:fetch-elem-helper id item fmt)
    (let* ([def-str (format "const elem = document.getElementById('~a');\n" id)]
          [req-str (js:make-request "localhost" 8000)]
          [fetch-str "fetch(req)\n"]
          [action-str (format "    .then((response) => {
    const t = response.text();
    if (!response.ok) {
        throw new Error(`HTTP error: ${response.status}`);
    }
    return t;\n})
    .then((str) => elem.~a = `~a`)
    .catch((err) => elem.~a = `${err}`);"
                            item (format fmt "str") item)])
      (string-append def-str req-str fetch-str action-str)))

(define (js:fetch id item fmt)
    (let ([id-str (symbol->string id)]
          [item-str (symbol->string item)]
          [fmt-str (scheme-str->js-str fmt)])
      (js:fetch-elem-helper id-str item-str fmt-str)))

;(display (js:fetch 'read 'textContent "success: ~a"))

(define (gencode-drawAxis symtable)
  (begin
    (hash-set! symtable 'drawAxis "")
  "function drawAxis(ctx) {
    ctx.beginPath();
    ctx.moveTo(0, h/2);
    ctx.lineTo(w, h/2);
    ctx.stroke();
}\n"))


(define (gencode-drawPoint symtable)
  (begin
    (hash-set! symtable 'drawPoint "")
"function drawPoint(ctx, x, y, r) {
    ctx.beginPath();
    ctx.arc(x, y, r, 0, 2*Math.PI, true);
    ctx.fill();
}\n"))


(define (gencode-scatter-init id symtable)
  (let ([canvas (symbol->string (gensym))]
        [context (symbol->string (gensym))])
    (hash-set! symtable (string->symbol (format "~a-canvas" id)) canvas)
    (hash-set! symtable (string->symbol (format "~a-context" id)) context)
(string-replace
    (format "const canvas = document.getElementById('~a');
const ~a = canvas.getContext('2d');\n
var w = canvas.width = canvas.clientWidth;
var h = canvas.height = canvas.clientHeight;\n\n" id context)
    "canvas" canvas)))

(define (gencode-scatter-1d host port symtable)
  (begin
    (hash-set! symtable 'scatter-1d "")
    "function scatter1(ctx, ys) {
    drawAxis(ctx);
    //if (ys===undefined) console.log('undefined detected.');
    let n = ys.length;
    for(let i = 0; i < n; ++i) {
        let x = i*w/(n - 1);
        let y = h*ys[i];
        drawPoint(ctx, x, y, 2);
    }
}\n\n"))

(define (gencode-scatter-plot-1d id host port symtable)
  (let* ([key (string->symbol (format "~a-context" id))]
         [ctx (hash-ref symtable key)]
         [hdr (symbol->string (gensym))]
         [req (symbol->string (gensym))]
         [defs (format "const ~a = new Headers();
const ~a = new Request('http://~a:~a',
    {method: 'GET', action: '/', headers: ~a});\n\n" hdr req host port hdr)]
         [fetch-str (format "// Fetch data
fetch(~a)
  .then((response) => {
     if (!response.ok) {
       throw new Error(`HTTP error: ${response.status}`);
     }
     return response.json();
  })
  .then((data) => {
      scatter1(~a, data);
  })
  .catch((err) => console.error(err));\n" req ctx)])
    (string-append defs fetch-str)))

(define (js:scatter id #:host [host "localhost"] #:port port symtable)
  (let ([sym-drawAxis (hash-ref symtable 'drawAxis 'not-found)]
        [sym-drawPoint (hash-ref symtable 'drawPoint 'not-found)]
        [sym-scatter-1d (hash-ref symtable 'scatter-1d 'not-found)])
    (let ([code-init (gencode-scatter-init id symtable)]
          [code-drawAxis
    (if (eq? sym-drawAxis 'not-found) (gencode-drawAxis symtable) "")]
          [code-drawPoint
           (if (eq? sym-drawPoint 'not-found) (gencode-drawPoint symtable) "")]
          [code-scatter-1d
           (if (eq? sym-scatter-1d 'not-found) (gencode-scatter-1d host port symtable) "")]
          [code-plot (gencode-scatter-plot-1d id host port symtable)])
      (string-append code-init code-drawAxis code-drawPoint code-scatter-1d code-plot))))


(define-syntax js:macro-helper
  (syntax-rules (scatter)
    [(_ (scatter arg ...))
     (js:scatter arg ...)]
    [(_ (scatter arg0 ...) (scatter arg1 ...) ...)
     (string-append (js:scatter arg0 ...)
                    (js:macro-helper (scatter arg1 ...) ...))]))


(define-syntax javascript
  (syntax-rules (scatter)
     [(_ (e arg ...))
    (let ([symtable (make-hash)])
         (js:macro-helper (e arg ... symtable)))]
    [(_ (e0 arg0 ...) (e1 arg1 ...) ...)
     (let ([symtable (make-hash)])
         (js:macro-helper (e0 arg0 ... symtable)
                          (e1 arg1 ... symtable) ...))]))


;(define (f s) (format "~a" (length (hash-keys s))))
;(define (g s) (begin (hash-set! s "key" (gensym)) ""))
;(javascript (g) (f))

;(scatter 'test #:port 8000 (make-hash))
(javascript (scatter 'test #:port 8000))
(display (javascript (scatter 'test #:port 8000) (scatter 'test2 #:port 8001) (scatter 'test3 #:port 8002)))