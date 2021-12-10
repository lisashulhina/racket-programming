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

(define xScale 1.0)
(define yScale 1.0)
(define xTranslate 0.0)
(define yTranslate 0.0)

(define xWorld 38.52955989354052)
(define yWorld 68.0884771616044)

(define xScreen 0)
(define yScreen 0)

; functions
(define (printPoly inPoly)
        (begin
          (define-values (whuh huh) (send inPoly get-datum))
          (display whuh)
          (newline))
)

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
  
  (draw-to-screen myPolygon)
  
 ; (send dc draw-path myPolygon)
  
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

     

    (cond [(= i (- polygonnumbers 1)) (printPoly myPolygon) ])
    (draw-to-screen myPolygon)
      
  ;  (send dc draw-path myPolygon)
      

    (cond
      [ (and (divisible? i 2) (< i 50)) ; did it only for (< i 100) because program takes too long and after 100 it's not even visible.
          (define myPolygon2 (new dc-path%))
          (send myPolygon2 move-to (+ (* i 30) 500) (+ (* i 30) 500))     
          (send myPolygon2 line-to (+ (* i 30) 600) (+ (* i 30) 500))
          (send myPolygon2 line-to (+ (* i 30) 600) (+ (* i 30) 600))
          (send myPolygon2 line-to (+ (* i 30) 500) (+ (* i 30) 600))
          (send myPolygon2 close)
          (send myPolygon2 scale 0.3 0.3)
          (send myPolygon2 translate xtranslate ytranslate)
          (draw-to-screen myPolygon2)
          
         ; (send dc draw-path myPolygon2)
          (for ([i (in-range (/ polygonnumbers 2))])
            (send myPolygon2 scale 0.9 0.9)
            (set! x (+ x (* x xrotate)))
            (set! x (+ y (* y yrotate)))
            (send myPolygon2 translate x y)
            (send myPolygon2 rotate (/ pi 30))
            (send myPolygon2 translate xtranslate ytranslate)

            (draw-to-screen myPolygon2)
            
          ;  (send dc draw-path myPolygon2)
       )])
      
    )
)


(define (draw-to-screen somePolygon)
  (send somePolygon scale xScale yScale)
  (send somePolygon translate xTranslate yTranslate)
  (send dc draw-path somePolygon)
  (send somePolygon translate (- xTranslate) (- yTranslate))
  (send somePolygon scale (/ 1 xScale) (/ 1 yScale))
)













; A bitmap
(define myTarget (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap myTarget]))

    (for ([j (in-range 1 601)])
      
      ; a drawing context
      (set! dc (new bitmap-dc% [bitmap myTarget]))
      
      ; sets the background 
      (send dc set-background "white")
      (send dc clear)

      (define myHsv (hsv random-color 1 1))
      (send dc set-brush (hsv->color myHsv) 'solid)
      (set! myHsv (hsv random-color2 1 1))
      (send dc set-pen (hsv->color myHsv) 1 'solid)

      (draw-polygons (cos (/ pi 10)) (sin (/ pi 10)) -143 -75 6000 j)

      (cond [(< j 150)
             (draw-polygons (sin (/ pi 10)) (- (cos (/ pi 10))) -110 20 100 j)
             (draw-polygons (- (sin (/ pi 10))) (sin (/ pi 10)) -125 125 100 j)
             (draw-polygons (cos (/ pi 10)) (sin (/ pi 10)) -35 15 100 j)
             ])
      
      (define outName (makeoutputname j  "myPolygon/myPolygon"))
      (send myTarget save-file outName 'png)

      (set! xScale (* 1.025 xScale))
      (set! yScale (* 1.025 yScale))

      (set! xTranslate (- xScreen (* xWorld xScale)))
      (set! yTranslate (- yScreen (* yWorld yScale)))      
    )

myTarget