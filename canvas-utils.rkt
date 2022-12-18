#lang racket

(require "js-utils.rkt")

(provide canvas:style)
(provide js:fetch-scatter)

(define (canvas:style #:width w #:height h)
     (format "canvas {\nwidth: ~a%;\nheight: ~a%;\nmargin: 20px auto;
display: flex;\nalign-items: center;\nborder: 1px solid black;
background-color: lightyellow;\n}\n" w h))

;(display (canvas:style #:width 40 #:height 80))

(define (js:fetch-scatter id #:host [host "localhost"] #:port [port 8000])
    (let ([req-str (js:make-request #:host host #:port port)])
(format "const canvas = document.getElementById('~a');
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
}

let ws = 1.0*w;
function scatter(v) {
    drawAxis();
    let n = v.length;
    for(let i = 0; i < n; ++i) {
        let x = i*ws/(n - 1);
        let y = h*v[i];
        drawPoint(x, y, 2);
    }
}

let v = new Float32Array();
// Fetch data
~a
fetch(req)
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
"  id req-str)))