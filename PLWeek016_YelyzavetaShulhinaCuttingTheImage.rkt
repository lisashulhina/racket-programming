#lang racket

; graphics library
(require racket/draw)
; color library
(require colors) 

(define imageWidth 1650)
(define imageHeight 1650)
(define random-color (random))
(define random-color2 (random))
(define random-rect-color (random 0 11))
(define random-rect-color2 (random 0 11))

; functions
(define (makeoutputname testnum prefix) ; only good up to 999
  (let ((suffix 
  (cond
    [(< testnum 10) (format "00~v.png" testnum)]
    [(< testnum 100) (format "0~v.png" testnum)]
    [ (format "~v.png" testnum)])))
    (string-append prefix suffix)))

(define (divisible? n x)
  (zero? (remainder n x)))

(define (draw-polygons xrotate yrotate xtranslate ytranslate polygonnumbers j)
  
  (define myPolygon (new dc-path%))
  (send myPolygon move-to 500 500)     
  (send myPolygon line-to 600 500)
  (send myPolygon line-to 600 600)
  (send myPolygon line-to 500 600)
  (send myPolygon close)
  (send dc draw-path myPolygon) ; draw polygon
  (define x 0)
  (define y 100)
  
  (draw-polygons-brunch x y myPolygon xrotate yrotate xtranslate ytranslate polygonnumbers j)
)

(define (draw-polygons-brunch x y myPolygon xrotate yrotate xtranslate ytranslate polygonnumbers j)
  
    (for ([i (in-range polygonnumbers)])
    (send myPolygon scale 0.9 0.9)
    (set! x (+ x (* x xrotate)))
    (set! x (+ y (* y yrotate)))
    (send myPolygon translate x y)
    (send myPolygon rotate (/ pi 30))
    (send myPolygon translate xtranslate ytranslate)
    (define myHsv (hsv random-color 1 1))
    (send dc set-brush (hsv->color myHsv) 'solid)
    (set! myHsv (hsv random-color2 1 1))
    (send dc set-pen (hsv->color myHsv) 1 'solid)
    (send dc draw-path myPolygon)
      

    (cond
      [ (and (divisible? i 10) (< i 100)) ; did it only for (< i 100) because program takes too long and after 100 it's not even visible.
          (define myPolygon2 (new dc-path%))
          (send myPolygon2 move-to (+ (* i 10) 500) (+ (* i 10) 500))     
          (send myPolygon2 line-to (+ (* i 10) 600) (+ (* i 10) 500))
          (send myPolygon2 line-to (+ (* i 10) 600) (+ (* i 10) 600))
          (send myPolygon2 line-to (+ (* i 10) 500) (+ (* i 10) 600))
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






; A bitmap
(define myTarget (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap myTarget]))
    (define rangeNumber 600)
    (define adjustNumber (exact-floor(/ imageWidth rangeNumber)))

    (for ([j (in-range rangeNumber)])

      ; A bitmap
      (set! myTarget (make-bitmap imageWidth imageHeight))
      (set! imageWidth (- imageWidth adjustNumber))
      (set! imageHeight (- imageHeight adjustNumber))
      
      ; a drawing context
      (set! dc (new bitmap-dc% [bitmap myTarget]))
      
      ; sets the background 
      (send dc set-background "white")
      (send dc clear)
      (define xrectangle 10)
      
      (define yrectangle 10)
      (define rectangleWidth 1630)
      (define rectangleHeight 1630)


      (define COLORS1 (list "Orange Red" "Pale Green" "yellow" "purple" "blue" "orange" "Light Pink" "Maroon" "Coral" "Yellow Green" "Turquoise" "Sky Blue"))

      (for ([i (in-range 82)])
        (send dc set-brush (list-ref COLORS1 random-rect-color) 'solid)
        (send dc set-pen (list-ref COLORS1 random-rect-color2) 1 'solid)
        (send dc draw-rectangle
              xrectangle yrectangle   
              rectangleWidth rectangleHeight)
        (set! xrectangle (+ xrectangle 10))
        (set! yrectangle (+ yrectangle 10))
        (set! rectangleWidth (- rectangleWidth 20))
        (set! rectangleHeight (- rectangleHeight 20))  
      )

      (send dc set-pen "black" 2 'solid) ; pen color   line_width   fill_mode
      (send dc set-brush (make-color 250 0 100) 'solid)  ; fill color   fill_mode

      (draw-polygons (cos (/ pi 10)) (sin (/ pi 10)) -35 15 3000 j)
      (draw-polygons (cos (/ pi 10)) (sin (/ pi 10)) -143 -75 3000 j)
      (draw-polygons (sin (/ pi 10)) (- (cos (/ pi 10))) -110 20 3000 j)
      (draw-polygons (- (sin (/ pi 10))) (sin (/ pi 10)) -125 125 3000 j)


      (define outName (makeoutputname j  "myPolygon/myPolygon"))
      (send myTarget save-file outName 'png)
      
    )

myTarget
