;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants

; Define Sprites
(define ALIEN-SHIP-WIDTH 60)
(define ALIEN-SHIP-HEIGHT 30)

(define ALIEN-SHIP-SPRITE (overlay/xy
                           (ellipse ALIEN-SHIP-WIDTH ALIEN-SHIP-HEIGHT "solid" "blue")
                           (- (/ ALIEN-SHIP-WIDTH 2) (/ ALIEN-SHIP-HEIGHT 2)) (- ALIEN-SHIP-HEIGHT ALIEN-SHIP-WIDTH)
                           (ellipse ALIEN-SHIP-HEIGHT ALIEN-SHIP-WIDTH "outline" "blue")))

(define TANK-WIDTH  60)
(define TANK-HEIGHT 45)

(define TANK-CHASIS-SPRITE (overlay (ellipse  TANK-WIDTH (/ TANK-HEIGHT 3) "solid" "black")
                                    (ellipse  TANK-WIDTH (/ TANK-HEIGHT 3) "outline" "green")))

(define TANK-ARMOR-TO-TANK-WIDTH-SCALE (/ TANK-WIDTH 4))
(define TANK-ARMOR-WIDTH (- TANK-WIDTH (* 2 TANK-ARMOR-TO-TANK-WIDTH-SCALE)))
(define TANK-ARMOR-HEIGHT (/ TANK-HEIGHT 2))

(define TANK-ARMOR-SPRITE (rectangle TANK-ARMOR-WIDTH TANK-ARMOR-HEIGHT "solid" "black"))

(define CANNON-WIDTH (/ TANK-WIDTH 5))
(define TANK-CANNON-SPRITE (rectangle CANNON-WIDTH TANK-HEIGHT "solid" "black"))

(define TANK-SPRITE (overlay/xy
                     (overlay/xy
                      TANK-CHASIS-SPRITE                
                      TANK-ARMOR-TO-TANK-WIDTH-SCALE (- (/ TANK-HEIGHT 3) (/ TANK-HEIGHT 2))
                      TANK-ARMOR-SPRITE)
                     (- (/ TANK-WIDTH 2) (/ CANNON-WIDTH 2)) (- TANK-ARMOR-HEIGHT TANK-HEIGHT)
                     TANK-CANNON-SPRITE))

(define BULLET-SPRITE (ellipse (* CANNON-WIDTH 0.75) (/ TANK-HEIGHT 2) "solid" "red"))

; Define Scene
(define MTS-WIDTH 200)
(define MTS-HEIGHT 500)
(define MTS (empty-scene MTS-WIDTH MTS-HEIGHT))

;; Data Definitions

(define-struct tank (x y dx))
;; Tank is a compoud structure (make-tank Integer Integer)
;; interp. It represents a tank located at x and y coordinates moving with some speed

(define T0 (make-tank 0 10 10))

#; (define (fn-for-tank tnk)
     (... (tank-x tnk)
          (tank-y tnk)
          (tank-dx tnk)))

;; Template rules used:
;; - compound: 2 fields



(define-struct bullet (x y dy))
;; Bullet is a compound structure (make-bullet Natural Natural Natural).
;; interp. Bullet represents bullet on the empty scene.

(define B0 (make-bullet 0 0 1))
(define B1 (make-bullet 1 13 12))

#; (define (fn-for-bullet blt)
     (... (bullet-x blt)
          (bullet-y blt)
          (bullet-dy blt)))

(define-struct alien-ship (x y dx dy))
;; Alien ship is a compound structure (make-alien-ship Natural Natural Integer Natural).
;; interp. Alien ship represents alien ship on the empty scene.

(define AS0 (make-alien-ship 0 0 -12 10))
(define AS1 (make-alien-ship 0 10 -12 10))

#; (define (fn-for-alien-ship als)
     (... (alien-ship-x als)
          (alien-ship-y als)
          (alien-ship-dx als)
          (alien-ship-dy als)))


;; ListOfEvents is one of:
;; - empty
;; - tank ListOfEvents
;; - bullet ListOfEvents
;; - alien-ship ListOfEvents
;; INVARIANT:
;;    - There is only one tank in ListOfEvents

(define LOE0 empty)
(define LOE1 (list (make-tank 0 12 1)))
(define LOE2 (list (make-tank 0 12 1) (make-bullet 0 9 1) (make-bullet 0 12 11)))
(define LOE3 (cons AS1 (cons AS0 LOE2)))

#;(define (fn-for-loe loe)
    (cond [(empty? loe) (...)]
          [else
           (... (first loe)
                (fn-for-loe (rest loe)))]))


;; Functions

(define (main tnk)
  (big-bang  tnk
    (on-tick next-tank 1)  ; Tank -> Tank
    (to-draw render-tank)  ; Tank -> Image
    (on-key handle-tank))) ; Tank KeyEvent -> Tank

;; Tank -> Tank
;; Produces the fallowing state of the tank.
(check-expect (next-tank (make-tank 10 0 10)) (make-tank 20 0 10))
(check-expect (next-tank (make-tank 10 0 -10)) (make-tank (/ CANNON-WIDTH 2) 0 -10))
(check-expect (next-tank (make-tank MTS-WIDTH 0 10)) (make-tank (- MTS-WIDTH (/ CANNON-WIDTH 2)) 0 10))
(check-expect (next-tank (make-tank 0 0 -10)) (make-tank (/ CANNON-WIDTH 2) 0 -10))

; (define (next-tank tnk) tnk) ; stub

(define (next-tank tnk)
  (make-tank (cond [(< (- MTS-WIDTH (/ CANNON-WIDTH 2)) (+ (tank-x tnk) (tank-dx tnk))) (- MTS-WIDTH (/ CANNON-WIDTH 2))]
                   [(> (/ CANNON-WIDTH 2) (+ (tank-x tnk) (tank-dx tnk))) (/ CANNON-WIDTH 2)]
                   [else (+ (tank-x tnk) (tank-dx tnk))])
             (tank-y tnk)
             (tank-dx tnk)))


;; Tank -> Iage
;; Renders tank from the tank data definition.

(check-expect (render-tank (make-tank 0 0 10)) (place-image TANK-SPRITE 0 0 MTS))
(check-expect (render-tank (make-tank 10 0 10)) (place-image TANK-SPRITE 10 0 MTS))

; (define (render-tank tank) MTS) ; stub
(define (render-tank tnk)
  (place-image TANK-SPRITE (tank-x tnk) (tank-y tnk) MTS))

;; Tank KeyEvent -> Tank
;; Changes direction of the tank depending on Key pressed.

(check-expect (handle-tank (make-tank 0 12 10) "left")   (make-tank 0 12 -10))
(check-expect (handle-tank (make-tank 0 12 -10) "left")  (make-tank 0 12 -10))
(check-expect (handle-tank (make-tank 0 12 10) "right")  (make-tank 0 12 10))
(check-expect (handle-tank (make-tank 0 12 -10) "right") (make-tank 0 12 10))


; (define (handle-tank tnk kvt) tnk) ; stub
(define (handle-tank tnk ke)
  (cond [(key=? ke "left") (make-tank (tank-x tnk) (tank-y tnk)
                                      (if (> 0 (tank-dx tnk))
                                          (tank-dx tnk)
                                          (* -1 (tank-dx tnk))))]
        [(key=? ke "right") (make-tank (tank-x tnk) (tank-y tnk)
                                       (if (< 0 (tank-dx tnk))
                                           (tank-dx tnk)
                                           (* -1 (tank-dx tnk))))]
        [else tnk]))


;; (main (make-tank (/ MTS-WIDTH 2) (- MTS-HEIGHT (/ TANK-HEIGHT 2)) 10))
