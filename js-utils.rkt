#lang racket

(provide js:make-request)
(provide js:fetch)

(define (scheme-str->js-str fmt)
    (string-replace fmt "~a" "${~a}"))

(define (js:make-request #:host [host "localhost"] #:port port)
    (format "const hdr = new Headers();\nconst req = new Request('http://~a:~a',\n\
    {method: 'GET', action: '/', headers: hdr});\n" host port))

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
