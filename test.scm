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

(display
   "const elem = document.getElementById('read'); \
    const hdr = new Headers(); \
    hdr.append('Accept', '*/*'); \
    hdr.append('Accept-Language', 'en-US,en;q=0.5'); \
    const req = new Request('http://localhost:8000', \
        { method: 'GET', \
          action: 'num', \
          headers: hdr, \
          mode: 'cors', \
          referer: 'client', \
          cache: 'no-cache', \
          origin: 'http://localhost' \
        });
    fetch(new Request('http://localhost:8000', \
     {method: 'GET', action: 'num'})) \
        .then((response) => { \
            const t = response.text(); \
            console.log(t); \
            if (!response.ok) { \
                throw new Error(`HTTP error: ${response.status}`); \
            } \
            return t; \
        }) \
        .then((str) => elem.textContent = `success: ${str}`) \
        .catch((err) => elem.textContent = `${err}`);")

(define (serve-page port)
    (let* ((doctype "<!DOCTYPE html>\n")
           (script-str
   "const elem = document.getElementById('read'); \
    const hdr = new Headers(); \
    hdr.append('Accept', '*/*'); \
    hdr.append('Accept-Language', 'en-US,en;q=0.5'); \
    const req = new Request('http://localhost:8000', \
        { method: 'GET', \
          action: 'num', \
          headers: hdr, \
          mode: 'cors', \
          referer: 'client', \
          cache: 'no-cache', \
          origin: 'http://localhost' \
        });
    fetch(new Request('http://localhost:8000', \
     {method: 'GET', action: 'num'})) \
        .then((response) => { \
            const t = response.text(); \
            console.log(t); \
            if (!response.ok) { \
                throw new Error(`HTTP error: ${response.status}`); \
            } \
            return t; \
        }) \
        .then((str) => elem.textContent = `success: ${str}`) \
        .catch((err) => elem.textContent = `${err}`);")
           (body `((p "I just read:")
                   (p (@ (id "read")))
                   (script ,script-str))))
    (display doctype port)
    (sxml->xml (fill-template "Fetch API test" body) port)))
    ;(sxml->xml (fill-template "Fetch API test" '((p "Hello"))) port)))
        

(define* (respond request #:optional body
            #:key
            (status 200)
            ;; TODO: change to content-type: 'text/plain for sending raw data
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
            (lambda (port)
                (cond ((eq? port 8001) (serve-page port))
                      ((eq? port 8000) (serve-num port))
                      (else (serve-page port))))))

(define (test-server request body)
    (respond body (request-headers request)))

(run-server test-server 'http '(#:port 8001))

