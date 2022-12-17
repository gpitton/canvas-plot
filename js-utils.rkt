#lang racket

(provide js:fetch)

(define (scheme-str->js-str fmt)
    (string-replace fmt "~a" "${~a}"))

(define (js:fetch-elem-helper id item fmt)
    (let* ([def-str (format "const elem = document.getElementById('~a');\n" id)]
          [hdr-str "const hdr = new Headers();\n"]
          [req-str (format "const req = new Request('http://localhost:8000',\n\
    {method: 'GET', action: '/', headers: hdr});\n")]
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
      (string-append def-str hdr-str req-str fetch-str action-str)))

(define (js:fetch id item fmt)
    (let ([id-str (symbol->string id)]
          [item-str (symbol->string item)]
          [fmt-str (scheme-str->js-str fmt)])
      (js:fetch-elem-helper id-str item-str fmt-str)))

;(display (js:fetch 'read 'textContent "success: ~a"))
