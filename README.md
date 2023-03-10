## In-browser plots for Racket

This is a work-in-progress, experimental library that I built mainly for teaching myself scheme and web technology.

The purpose of this library is to provide a way to plot data on the browser, from Racket.

As an example, the following scheme code:
```scheme
`((p "We will use canvas to draw a scatter plot (think of a time series)")
   (canvas ((id "fig-1")))
   (style ,(canvas:style #:width 40 #:height 80))
   (p "Below, we also send the x-coordinates for the scatter plot.")
   (canvas ((id "fig-2")))
   (p "We also support time-dependent plots:")
   (canvas ((id "fig-3")))
   (script ,(js:plot (scatter    'fig-1 #:port 8001)
                     (scatter-2d 'fig-2 #:port 8002)
                     (dynamic
                       (scatter-2d 'fig-3 #:port 8002)
                       (with #:refresh-rate 1000)))))
```
generates the HTML page below:
![Screen-shot from browser](screenshot.png)
Where the third plot changes with time.
The only new part is the S-expression starting with `js:plot`.

For a more extensive example, check out the source of `test-scatter.rkt`, and run it with: `racket test-scatter.rkt`, then open the url `localhost:8000` with your browser.

At some point I would also like to port this library to rnrs Scheme. A first prototype (using Guile) is in `test.scm`.

### References
- [Guide to the canvas API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API)
- [Canvas deep dive](https://joshondesign.com/p/books/canvasdeepdive/toc.html)

### To do
- If there is any attribute in a canvas tag that is given a style, such as:
```scheme
(canvas ((id "fig-1") (width "300") (height "200")))
(style ,(canvas:style #:id "fig-1"))
```
then any call to `js:plot` should be aware of the properties of the
`"fig-1"` element and add a `width` and `height` attribute to the style.

