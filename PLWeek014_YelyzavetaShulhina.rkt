#lang racket
; graphics library
(require racket/draw)
; color library
(require colors) 

(define imageWidth 1650)
(define imageHeight 1650)

; A bitmap
(define myTarget (make-bitmap imageWidth imageHeight))
 ; a drawing context
(define dc (new bitmap-dc% [bitmap myTarget]))

; sets the background 
(send dc set-background "white")
(send dc clear)


(send dc set-pen "black" 2 'solid) ; pen color   line_width   fill_mode
(send dc set-brush "white" 'solid)  ; fill color   fill_mode

(define xrectangle 10)
(define yrectangle 10)
(define rectangleWidth 1630)
(define rectangleHeight 1630)

(define (divisible? n x)
  (zero? (remainder n x)))

(define COLORS1 (list "Orange Red" "Pale Green" "yellow" "purple" "blue" "orange" "Light Pink" "Maroon" "Coral" "Yellow Green" "Turquoise" "Sky Blue"))

(for ([i (in-range 82)])
  (send dc set-brush (list-ref COLORS1 (random 0 11)) 'solid)
  (send dc set-pen (list-ref COLORS1 (random 0 11)) 1 'solid)
  (send dc draw-rectangle
        xrectangle yrectangle   
        rectangleWidth rectangleHeight)
  (set! xrectangle (+ xrectangle 10))
  (set! yrectangle (+ yrectangle 10))
  (set! rectangleWidth (- rectangleWidth 20))
  (set! rectangleHeight (- rectangleHeight 20))  
 )


(display "A rectangle that fills the screen with a white background color:\n")
;myTarget ; display image

(send dc set-pen "black" 2 'solid) ; pen color   line_width   fill_mode
(send dc set-brush (make-color 250 0 100) 'solid)  ; fill color   fill_mode

; defining polygon that contains more than three points ( rectangle starting at (40,30) )
(define myPolygon (new dc-path%))
(send myPolygon move-to 700 700)     
(send myPolygon line-to 800 700)
(send myPolygon line-to 800 800)
(send myPolygon line-to 700 800)
(send myPolygon close)

(send dc draw-path myPolygon) ; draw polygon

(define x 0)
(define y 100)

(define (draw-polygons xrotate yrotate xtranslate ytranslate polygonnumbers)
  
  (set! myPolygon (new dc-path%))
  (send myPolygon move-to 700 700)     
  (send myPolygon line-to 800 700)
  (send myPolygon line-to 800 800)
  (send myPolygon line-to 700 800)
  (send myPolygon close)
  
  (draw-polygons-brunch xrotate yrotate xtranslate ytranslate polygonnumbers)
)

(define (draw-polygons-brunch xrotate yrotate xtranslate ytranslate polygonnumbers)
    (for ([i (in-range polygonnumbers)]) ; 12500 for each brunch so 50 000 polygons
    (send myPolygon scale 0.9 0.9)
    (set! x (+ x (* x xrotate)))
    (set! x (+ y (* y yrotate)))
    (send myPolygon translate x y)
    (send myPolygon rotate (/ pi 30))
    (send myPolygon translate xtranslate ytranslate)
    (define myHsv (hsv (random) 1 1))
    (send dc set-brush (hsv->color myHsv) 'solid)
    (set! myHsv (hsv (random) 1 1))
    (send dc set-pen (hsv->color myHsv) 1 'solid)
    (send dc draw-path myPolygon)
    (cond
      [ (and (divisible? i 10) (< i 100))
          (define myPolygon2 (new dc-path%))
          (send myPolygon2 move-to (+ (* i 10) 700) (+ (* i 10) 700))     
          (send myPolygon2 line-to (+ (* i 10) 800) (+ (* i 10) 700))
          (send myPolygon2 line-to (+ (* i 10) 800) (+ (* i 10) 800))
          (send myPolygon2 line-to (+ (* i 10) 700) (+ (* i 10) 800))
          (send myPolygon2 close)
          (send myPolygon2 scale 0.5 0.5)
          (send myPolygon2 translate xtranslate ytranslate)
          (send dc draw-path myPolygon2)
          (for ([i (in-range polygonnumbers)])
            (send myPolygon2 scale 0.9 0.9)
            (set! x (+ x (* x xrotate)))
            (set! x (+ y (* y yrotate)))
            (send myPolygon2 translate x y)
            (send myPolygon2 rotate (/ pi 30))
            (send myPolygon2 translate xtranslate ytranslate)
            (send dc draw-path myPolygon2)
       )])
    )
)

(draw-polygons (cos (/ pi 10)) (sin (/ pi 10)) -35 51 12500)
(draw-polygons (cos (/ pi 10)) (sin (/ pi 10)) -143 -36 12500)
(draw-polygons (sin (/ pi 10)) (- (cos (/ pi 10))) -110 60 12500)
(draw-polygons (- (sin (/ pi 10))) (sin (/ pi 10)) -120 163 12500)


myTarget

; save image as myPolygon.png 
(send myTarget save-file "myPolygon.png" 'png) 