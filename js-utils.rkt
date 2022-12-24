#lang racket

(provide js:plots)


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


(define (gencode-scatter dim host port symtable)
  (if (eq? dim 1)
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
}\n\n")
      (begin
        (hash-set! symtable 'scatter-2d "")
        "function scatter2(ctx, data) {
    drawAxis(ctx);
    let xs = data[0];
    let ys = data[1];
    //if (ys===undefined) console.log('undefined detected.');
    let n = ys.length;
    for(let i = 0; i < n; ++i) {
        let x = w*xs[i];
        let y = h*ys[i];
        drawPoint(ctx, x, y, 2);
    }
}\n\n")))


(define (gencode-scatter-plot dim id host port symtable)
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
      scatter~a(~a, data);
  })
  .catch((err) => console.error(err));\n" req dim ctx)])
    (string-append defs fetch-str)))


(define (js:scatter-helper dim id host port symtable)
  (let ([sym-key (string->symbol (format "scatter-~ad" dim))])
    (let ([sym-drawAxis (hash-ref symtable 'drawAxis 'not-found)]
          [sym-drawPoint (hash-ref symtable 'drawPoint 'not-found)]
          [sym-scatter (hash-ref symtable sym-key 'not-found)])
      (let ([code-init (gencode-scatter-init id symtable)]
            [code-drawAxis (if (eq? sym-drawAxis 'not-found)
                               (gencode-drawAxis symtable)
                               "")]
            [code-drawPoint (if (eq? sym-drawPoint 'not-found)
                                (gencode-drawPoint symtable)
                                "")]
            [code-scatter (if (eq? sym-scatter 'not-found)
                              (gencode-scatter dim host port symtable)
                              "")]
            [code-plot (gencode-scatter-plot dim id host port symtable)])
        (string-append code-init code-drawAxis code-drawPoint code-scatter code-plot)))))


(define (js:scatter id #:host [host "localhost"] #:port port symtable)
  (js:scatter-helper 1 id host port symtable))


(define (js:scatter-2d id #:host [host "localhost"] #:port port symtable)
  (js:scatter-helper 2 id host port symtable))


(define-syntax js:macro-helper
  (syntax-rules (scatter scatter-2d)
    ;; base cases: macro called with a single argument
    [(_ (scatter arg ...))
     (js:scatter arg ...)]
    [(_ (scatter-2d arg ...))
     (js:scatter-2d arg ...)]
    ;; recursive cases: macro called with more than one arguments
    [(_ (scatter arg0 ...) e0 ...)
     (string-append (js:scatter arg0 ...)
                    (js:macro-helper e0 ...))]
    [(_ (scatter-2d arg0 ...) e0 ...)
     (string-append (js:scatter-2d arg0 ...)
                    (js:macro-helper e0 ...))]))


(define-syntax js:plots
  (syntax-rules ()
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
;(js:plots (scatter 'test #:port 8000))
;(display (js:plots (scatter 'test #:port 8000) (scatter 'test2 #:port 8001) (scatter 'test3 #:port 8002)))