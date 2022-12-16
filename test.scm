(use-modules (ice-9 threads))
(use-modules (sxml simple))
(use-modules (web server)
             (web request)
             (web response))


;(define serve-num random:uniform)
(define (serve-num port)
    (display (random:uniform) port))

(define (fill-template title body)
    `(html (head (title ,title))
           (body ,@body)))

(define (serve-page port)
    (let* ((doctype "<!DOCTYPE html>\n")
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
                   ;; empty string necessary to force the parser
                   ;; to add a </p> element.
                   (p (@ (id "read")) "")
                   (script ,script-str))))
    (display doctype port)
    (sxml->xml (fill-template "Fetch API test" body) port)))

(define* (respond-num request #:optional body
            #:key
            (status 200)
            (content-type 'text/plain)
            ;(content-type-params '((charset . "utf-8")))
            (content-type-params '())
            ;(access-control-allow-origin "*")
            ;(extra-headers '((access-control-allow-origin . "*"))))
            (extra-headers '()))
    (values (build-response
             #:code status
             #:headers `((content-type . (,content-type ,@content-type-params))
                         ,@extra-headers))
            (lambda (port) (serve-num port))))

(define* (respond request #:optional body
            #:key
            (status 200)
            (content-type 'text/html)
            ;(content-type-params '((charset . "utf-8")))
            (content-type-params '())
            ;(access-control-allow-origin "*")
            ;(extra-headers '((access-control-allow-origin . "*"))))
            (extra-headers '()))
    (values (build-response
             #:code status
             #:headers `((content-type . (,content-type ,@content-type-params))
                         ,@extra-headers))
            (lambda (port) (serve-page port))))

(define (test-server-num request body)
    (respond-num body (request-headers request)))
(define (test-server request body)
    (respond body (request-headers request)))

;; TODO provide a way to quit execution
(parallel
    (run-server test-server-num 'http '(#:port 8000))
    (run-server test-server 'http '(#:port 8001)))

