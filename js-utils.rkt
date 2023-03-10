#lang racket

(provide js:plot)


;; pconf stands for "plot configuration"
(struct pconf (id dim host port refresh-rate)
  #:transparent)


(define (gen-draw-axis symtable)
  (begin
    (hash-set! symtable 'draw-axis "")
    "function drawAxis(ctx) {
    ctx.beginPath();
    ctx.moveTo(0, h/2);
    ctx.lineTo(w, h/2);
    ctx.stroke();
}\n"))


(define (gen-draw-point symtable)
  (begin
    (hash-set! symtable 'draw-point "")
    "function drawPoint(ctx, x, y, r) {
    ctx.beginPath();
    ctx.arc(x, y, r, 0, 2*Math.PI, true);
    ctx.fill();
}\n"))


;; TODO use a new macro with (symbol->string (gensym)) (hash-set! symtable sym ...
(define (gen-plot-init id symtable)
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


(define (gen-scatter-def dim symtable)
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


(define (gen-fetch-def dim ctx req symtable)
  (let* ([key (string->symbol (format "fetch-scatter-~a" dim))]
         [val (hash-ref symtable key 'not-found)])
    (if (eq? val 'not-found)
        (begin
          (hash-set! symtable key "")
          (format "function fetchScatter~a(ctx, req) {
    fetch(req)
    .then((response) => {
        if (!response.ok) {
            throw new Error(`HTTP error: ${response.status}`);
        }
        return response.json();
    })
    .then((data) => {
        scatter~a(ctx, data);
    })
    .catch((err) => console.error(err));\n}\n\n" dim dim))
        "\n")))


(define (gen-fetch-call params ctx req)
  (if (zero? (pconf-refresh-rate params))
      (format "fetchScatter~a(~a, ~a);\n\n" (pconf-dim params) ctx req)
      (format "window.addEventListener('load', () => setInterval(fetchScatter~a, ~a, ~a, ~a) );"
              (pconf-dim params) (pconf-refresh-rate params) ctx req)))


(define (gen-scatter-plot params symtable)
  (let* ([key-ctx (string->symbol (format "~a-context" (pconf-id params)))]
         [ctx (hash-ref symtable key-ctx)]
         [hdr (symbol->string (gensym))]
         [req (symbol->string (gensym))]
         [req-str (format "const ~a = new Headers();
const ~a = new Request('http://~a:~a',
    {method: 'GET', action: '/', headers: ~a});\n\n" hdr req
                                                     (pconf-host params)
                                                     (pconf-port params)
                                                     hdr)]
         [fetch-def (gen-fetch-def (pconf-dim params) ctx req symtable)]
         [fetch-call (gen-fetch-call params ctx req)])
    (string-append req-str fetch-def fetch-call)))


;; TODO add optional parameter #:clf (#t or #f) that clears the current figure
;; refresh-rate = 0 means static plot.
(define (js:scatter-helper dim id #:host [host "localhost"]
                           #:port port
                           #:refresh-rate [refresh-rate 0]
                           symtable)
  (let ([sym-key (string->symbol (format "scatter-~ad" dim))])
    (let ([sym-draw-axis (hash-ref symtable 'draw-axis 'not-found)]
          [sym-draw-point (hash-ref symtable 'draw-point 'not-found)]
          [sym-scatter (hash-ref symtable sym-key 'not-found)]
          [params (pconf id dim host port refresh-rate)])
      (let ([code-init (gen-plot-init id symtable)]
            [code-draw-axis (if (eq? sym-draw-axis 'not-found)
                               (gen-draw-axis symtable)
                               "")]
            [code-draw-point (if (eq? sym-draw-point 'not-found)
                                (gen-draw-point symtable)
                                "")]
            [code-scatter (if (eq? sym-scatter 'not-found)
                              (gen-scatter-def dim symtable)
                              "")]
            [code-plot (gen-scatter-plot params symtable)])
        (string-append code-init code-draw-axis code-draw-point code-scatter code-plot)))))


;; TODO for all the following macros: check if they are
;; igienic and if not switch to syntax-case
(define-syntax js:macro-helper
  (syntax-rules (scatter scatter-2d)
    ;; base cases: macro called with a single argument
    [(_ (scatter arg ...))
     (js:scatter-helper 1 arg ...)]
    [(_ (scatter-2d arg ...))
     (js:scatter-helper 2 arg ...)]
    ;; recursive cases: macro called with more than one arguments
    [(_ (scatter arg0 ...) e0 ...)
     (string-append (js:scatter-helper 1 arg0 ...)
                    (js:macro-helper e0 ...))]
    [(_ (scatter-2d arg0 ...) e0 ...)
     (string-append (js:scatter-helper 2 arg0 ...)
                    (js:macro-helper e0 ...))]))


(define-syntax js:match-dyn
  (syntax-rules (dynamic with)
    ;; base cases
    [(_ (dynamic (e arg ...) ... (with cs ...) st))  ;; the last element is the symtable
     (js:macro-helper (e arg ... cs ... st) ...)]
    [(_ e)
     (js:macro-helper e)]
    ;; recursive cases
    [(_ (dynamic (e arg ...) ... (with cs ...) st) e2 ...)
     (string-append (js:macro-helper (e arg ... cs ... st) ...)
                    (js:match-dyn e2 ...))]
    [(_ e1 e2 ...)
     (string-append (js:macro-helper e1)
                    (js:match-dyn e2 ...))]))


(define-syntax js:plot
  (syntax-rules ()
    [(_ (e arg ...))
     (let ([symtable (make-hash)])
       (js:match-dyn (e arg ... symtable)))]
    [(_ (e0 arg0 ...) (e1 arg1 ...) ...)
     (let ([symtable (make-hash)])
       (js:match-dyn (e0 arg0 ... symtable)
                     (e1 arg1 ... symtable) ...))]))


;; TODO turn these commented lines into tests
;(scatter 'test #:port 8000 (make-hash))
;(js:plot (scatter 'test #:port 8000))
;(display (js:plot (scatter 'test #:port 8000) (scatter 'test2 #:port 8001) (scatter 'test3 #:port 8002)))
;(display (js:plot (scatter 'fig-1 #:port 8001) (dynamic (scatter 'fig2 #:port 8002) (with #:refresh-rate 2000))))
;(display (js:plot (scatter 'fig-1 #:port 8001) (dynamic (scatter 'fig2 #:port 8002) (scatter 'fig-4 #:port 8004) (with #:refresh-rate 2000)) (scatter-2d 'fig-3 #:port 8003)))
