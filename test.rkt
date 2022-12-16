#lang racket
(require web-server/servlet)
(require web-server/servlet-env)


(define (serve-num req)
    (response/output
         (lambda (op) (write (random) op))
         #:mime-type #"text/plain; charset=utf-8"))

(define (fill-template title body)
    `(html (head (title ,title))
           (body ,@body)))

;; TODO generate javascript correctly:
;;    1. fix the escape of > to &gt
;;    2. scheme-javascript compiler
(define (serve-page req)
    (let* (;(doctype "<!DOCTYPE html>\n")
           (script-str
   "const elem = document.getElementById('read');
    const hdr = new Headers();
    const req = new Request('http://localhost:8000',
        { method: 'GET',
          action: 'num',
          headers: hdr,
        });
    fetch(new Request('http://localhost:8000',
     {method: 'GET', action: 'num'}))
        .then((response) => {
            const t = response.text();
            console.log(t);
            if (!response.ok) {
                throw new Error(`HTTP error: ${response.status}`);
            }
            return t;
        })
        .then((str) => elem.textContent = `success: ${str}`)
        .catch((err) => elem.textContent = `${err}`);")
           (body `((p "I just read:")
                   (p ((id "read")))
                   (script ,script-str))))
    (response/xexpr (fill-template "Fetch API test" body))))


;; TODO provide a way to quit execution/run in parallel
#|
(serve/servlet serve-num
               #:port 8000
               #:servlet-path "/"
               #:command-line? #t
               #:stateless? #f)
|#
(serve/servlet serve-page
               #:port 8001
               #:servlet-path "/"
               #:command-line? #t
               #:stateless? #f)

