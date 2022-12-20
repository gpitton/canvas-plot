#lang racket

(require "js-utils.rkt")

(provide canvas:style)
(provide js:fetch-scatter)
(provide js:fetch-scatter-2d)

(define (canvas:style #:width w #:height h)
     (format "canvas {\nwidth: ~a%;\nheight: ~a%;\nmargin: 20px auto;
display: flex;\nalign-items: center;\nborder: 1px solid black;
background-color: lightyellow;\n}\n" w h))

;(display (canvas:style #:width 40 #:height 80))

(define (js:scatter-base id)
    (let ([code-str (format "const canvas = document.getElementById('~a');
const ctx = canvas.getContext('2d');

var w = canvas.width = canvas.clientWidth;
var h = canvas.height = canvas.clientHeight;

function drawAxis() {
    ctx.beginPath();
    ctx.moveTo(0, h/2);
    ctx.lineTo(w, h/2);
    ctx.stroke();
}

function drawPoint(x, y, r) {
    ctx.beginPath();
    ctx.arc(x, y, r, 0, 2*Math.PI, true);
    ctx.fill();
}" id)])
      (string-replace (string-replace code-str "canvas" (symbol->string (gensym)))
                      "ctx" (symbol->string (gensym)))))

(define (js:scatter-draw dim)
    (let ([args (if (eq? dim 2) "xs, ys" "ys")]
          [xexp (if (eq? dim 2) "w*xs[i]" "i*w/(n - 1)")])
         (format "
function scatter(~a) {
    drawAxis();
    console.log(ys);
    console.log(ys.length);
    let n = ys.length;  // TODO assert length xs == length ys
    for(let i = 0; i < n; ++i) {
        let x = ~a;
        let y = h*ys[i];
        drawPoint(x, y, 2);
    }
}" args xexp)))


(define (js:fetch-scatter id #:host [host "localhost"] #:port [port 8000])
    (let ([base-str (js:scatter-base id)]
          [draw-str (js:scatter-draw 1)])
      (let-values
          ([(req-str req-name) (js:make-request #:host host #:port port)])
    (format "~a\n\n~a\n~a\n
// Fetch data
fetch(~a)
  .then((response) => {
     if (!response.ok) {
       throw new Error(`HTTP error: ${response.status}`);
     }
     return response.json();
  })
  .then((data) => {
      scatter(data);
  })
  .catch((err) => console.error(err));

// width of scatter plot.
" base-str draw-str req-str req-name))))

(define (js:fetch-scatter-2d id #:host [host "localhost"] #:port [port 8000])
    (let ([base-str (js:scatter-base id)]
          [draw-str (js:scatter-draw 2)])
      (let-values
          ([(req-str req-name) (js:make-request #:host host #:port port)])
    (format "~a\n\n~a\n~a\n
// Fetch data
fetch(~a)
  .then((response) => {
     if (!response.ok) {
       throw new Error(`HTTP error: ${response.status}`);
     }
     return response.json();
  })
  .then((data) => {
      scatter(data[0], data[1]);
  })
  .catch((err) => console.error(err));

// width of scatter plot.
" base-str draw-str req-str req-name))))