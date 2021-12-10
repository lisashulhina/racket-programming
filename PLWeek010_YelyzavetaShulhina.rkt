#lang racket

; graphics library
(require racket/draw)
; color library
(require colors) 

(define imageWidth 512)
(define imageHeight 288)

; A bitmap
(define myTarget (make-bitmap imageWidth imageHeight))
 ; a drawing context
(define dc (new bitmap-dc% [bitmap myTarget]))

; sets the background 
(send dc set-background "white")
(send dc clear)

(send dc set-pen "green" 2 'solid) ; pen color   line_width   fill_mode
(send dc set-brush "white" 'solid)  ; fill color   fill_mode

; full-screen rectangle 
(send dc draw-rectangle
      0 0   ; Top-left at (226, 134), center
      512 288) ; 80 pixels wide and 20 pixels high

(display "A rectangle that fills the screen with a white background color:\n")
myTarget ; display image




(send dc set-pen "black" 2 'solid) ; pen color   line_width   fill_mode
(send dc set-brush (make-color 250 0 100) 'solid)  ; fill color   fill_mode

; defining polygon that contains more than three points ( rectangle starting at (40,30) )
(define myPolygon (new dc-path%))
(send myPolygon move-to 40 30) 
(send myPolygon line-to 40 35)
(send myPolygon line-to 30 35)
(send myPolygon line-to 30 30)
(send myPolygon close)

(send dc draw-path myPolygon) ; draw polygon
(display "\nmyPolygon added:\n")
myTarget ; display image


; polygon translate
(send myPolygon translate 10 10) 
(send dc draw-path myPolygon) 
(display "\nmyPolygon translated:\n")
myTarget 


; polygon scale
(send myPolygon scale 4 4)  
(send dc draw-path myPolygon) 
(display "\nmyPolygon scaled: \n")
myTarget 


; polygon translated back
(send myPolygon translate -10 -10) 
(send dc draw-path myPolygon) 
(display "\nmyPolygon translated back: \n")
myTarget



; recursively rotate polygon and change color of polygon
(define (rotate-polygon-call myPolygon colorNum)
  (cond
    [ (< colorNum 0.999)
      (send myPolygon rotate 0.0523599) ; rotate by 3 degree (number converted to radians)
      (define myHsv (hsv colorNum 1 1))
      (send dc set-brush (hsv->color myHsv) 'solid) ; fill color   fill_mode
      (send dc draw-path myPolygon)
      (rotate-polygon-call myPolygon (+ colorNum 0.008325)) ; we will have 120 figures, sinwe we rotate by 3 degree 360/3=120. 0.999/120=0.008325 for color change
  ]
 )
)

;call recursive function with 0 for color start as a default
(define (rotate-polygon myPolygon)
  (rotate-polygon-call myPolygon 0)
)



(rotate-polygon myPolygon)
(display "\nRotated myPolygon: \n")
myTarget


; save image as myPolygon.png 
(send myTarget save-file "myPolygon.png" 'png) 