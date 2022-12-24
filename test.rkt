#lang racket
(require xml)
(require web-server/servlet)
(require web-server/servlet-env)


(define (serve-num req)
  (response/output
   (lambda (op) (write (random) op))
   #:mime-type #"text/plain; charset=utf-8"
   #:headers (list (make-header #"Access-Control-Allow-Origin" #"*"))))

(define (fill-template title body)
  `(html (head (title ,title))
         (body ,@body)))

;; TODO find a better way to generate javascript
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
    (response/output
     (lambda (op) (parameterize ([current-unescaped-tags (cons 'id html-unescaped-tags)])
                    (write-xexpr (fill-template "Fetch API test" body) op))))))

;; main loop
(define text-server
  (thread (lambda ()
            (serve/servlet serve-num
                           #:port 8000
                           #:servlet-path "/"
                           #:command-line? #t))))

(define html-server
  (thread (lambda ()
            (serve/servlet serve-page
                           #:port 8001
                           #:servlet-path "/"
                           #:command-line? #t))))

(sleep 120)
(kill-thread text-server)
(kill-thread html-server)
