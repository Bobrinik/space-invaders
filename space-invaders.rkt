;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants

; Define Sprites
(define ALIEN-SHIP-WIDTH 30)
(define ALIEN-SHIP-HEIGHT 15)

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

(define BULLET-SPEED -12)

; Define Scene
(define MTS-WIDTH 200)
(define MTS-HEIGHT 500)
(define MTS (empty-scene MTS-WIDTH MTS-HEIGHT))

;; Constants: Alien Ship
(define ALIEN-SHIP-DX 6)
(define ALIEN-SHIP-DY 2.5)

(define HIT-RANGE-X (/ (image-width ALIEN-SHIP-SPRITE) 2))
(define HIT-RANGE-Y (/ (image-height ALIEN-SHIP-SPRITE) 2))

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

(define-struct game-over (text))
;; GameOver structure is a compound data structure (make-game-over String)
;; interp: GameOver tells that the game is over.

(define GO (make-game-over "GAME OVER 🦍"))

(define (fn-for-go go)
  (... (game-over-text go)))

;; Template rules:
;; - compound: 1 field

;; Event is one of:
;; - tank
;; - bullet
;; - alien-ship
;; - game-over
;; interp. Event represents possible data definition displayable on empty-scene

(define ET  (make-tank 0 0 1))
(define EB  (make-bullet 0 12 10))
(define EAS (make-alien-ship 13 12 10 12))
(define GO2 (make-game-over "Game over"))

#;(define (fn-for-event evt)
    (cond [(tank? evt) (fn-for-tank evt)]
          [(bullet? evt) (fn-for-bullet evt)]
          [(alien-ship? evt) (fn-for-alien-ship evt)]
          [(game-over? evt) (fn-for-go evt)]))

;; ListOfEvents is one of:
;; - empty
;; - Event ListOfEvents
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
    (on-tick tock)    ; ListOfEvents -> ListOfEvents
    (to-draw render-loe)  ; ListOfEvents -> Image
    (on-key handle-loe))) ; ListOfEvents KeyEvent -> ListOfEvents

;; ListOfEvents -> ListOfEvents
;; Works as an assembly function.
(define (tock loe)
  (if (or (is-game-over? loe ) (game-over? (first loe)))
      (list GO)
      (bullet-collisions
       (if (alien-ship-arrives? loe)
           (cons (create-alien-ship ALIEN-SHIP-DX ALIEN-SHIP-DY) (next-loe loe))
           (next-loe loe))))) ; stub

;; ListOfEvents -> Boolean
;; Returns GameOver if the game over condition are reached

(check-expect (is-game-over? empty) #f)
(check-expect (is-game-over? (list (make-tank 10 0 10))) #f)
(check-expect (is-game-over? (list (make-tank 10 0 10)  (make-alien-ship 10 0 10 10))) #f)
(check-expect (is-game-over? (list (make-tank 10 0 10)  (make-alien-ship 10 MTS-HEIGHT 10 10))) #t)
(check-expect (is-game-over? (list (make-tank 10 0 10)  (make-alien-ship 10 0 10 10) (make-alien-ship 10 (+ MTS-HEIGHT 2) 10 10))) #t)

(define (is-game-over? evnt)
  (cond [(empty? evnt) #f]
        [else
         (if (and (alien-ship? (first evnt))
                  (alien-ship-touch-down? (first evnt)))
             #t
             (is-game-over? (rest evnt)))]))


;; alien-ship -> Boolean
;; Returns true if alien ship has reached ground level.
(check-expect (alien-ship-touch-down? (make-alien-ship 10 (- MTS-HEIGHT 2) 10 10)) #f)
(check-expect (alien-ship-touch-down? (make-alien-ship 10 MTS-HEIGHT 10 10)) #t)
(check-expect (alien-ship-touch-down? (make-alien-ship 10 (+ MTS-HEIGHT 2) 10 10)) #t)

; (define (alien-ship-touch-down? as) #f) ; stub

(define (alien-ship-touch-down? als)
  (>= (alien-ship-y als) MTS-HEIGHT))


;; ListOfEvents -> ListOfEvents
;; Collides bullets with alien ships.

(check-expect (bullet-collisions empty) empty)
(check-expect (bullet-collisions (list (make-alien-ship 1 12 1 1))) (list (make-alien-ship 1 12 1 1)))
(check-expect (bullet-collisions (list (make-alien-ship 1 12 1 1) (make-alien-ship 1 12 1 1) (make-bullet 1 12 10))) (list (make-alien-ship 1 12 1 1)))
(check-expect (bullet-collisions (list (make-bullet 1 12 1) (make-alien-ship 1 12 1 1))) empty)
(check-expect (bullet-collisions (list (make-bullet 1 12 1) (make-alien-ship 1 13 1 1) (make-alien-ship 1 12 1 1))) (list (make-alien-ship 1 13 1 1)))
(check-expect (bullet-collisions (list (make-bullet 1 13 1) (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 13) 1) (make-alien-ship 1 13 1 1))) (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 13) 1)))

; (define (bullet-collisions loe) loe) ; stub
(define (bullet-collisions loe)
  (cond [(empty? loe) empty]
        [else
         (cond [(alien-ship? (first loe))
                (alien-ship-hit (first loe)
                                (bullet-collisions (rest loe)))]
               [(bullet? (first loe))
                (bullet-hits (first loe)
                             (bullet-collisions (rest loe)))]
               [else
                (cons (first loe) (bullet-collisions (rest loe)))])]))


;; alien-ship ListOfEvents -> ListOfEvents
;; Alien ship is hit by some boullet in the ListOfEvents

(check-expect (alien-ship-hit  (make-alien-ship 1 12 1 1) (list (make-bullet 1 12 1))) empty)

(check-expect (alien-ship-hit  (make-alien-ship 1 12 1 1) (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 12) 1)))
              (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 12) 1) (make-alien-ship 1 12 1 1)))

(check-expect (alien-ship-hit  (make-alien-ship 1 12 1 1) (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 1) 1) (make-bullet 1 12 1)))
              (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 1) 1)))

(check-expect (alien-ship-hit  (make-alien-ship 1 12 1 1) (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 1) 1) (make-alien-ship 1 12 1 1) (make-bullet 1 12 1)))
              (list (make-bullet (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 1) 1)
                    (make-alien-ship 1 12 1 1)))

; (define (alien-ship-hit as loe) loe) ; stub

(define (alien-ship-hit as loe)
  (cond [(empty? loe) (cons as empty)]
        [else
         (if (and (bullet? (first loe))
                  (bullet-touches-alien-ship? (first loe) as))
             (rest loe)
             (cons (first loe) (alien-ship-hit as (rest loe))))]))


;; bullet ListOfEvents -> ListOfEvents
;; Bullet hits some alien ship in the ListOfEvents


(check-expect (bullet-hits (make-bullet 1 12 1) (list (make-alien-ship 1 12 1 1))) empty)
(check-expect (bullet-hits (make-bullet 1 12 1) empty) (list (make-bullet 1 12 1)))
(check-expect (bullet-hits (make-bullet 1 12 1) (list (make-alien-ship (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 12) 1 1)))
              (list (make-alien-ship (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 12) 1 1) (make-bullet 1 12 1)))

(check-expect (bullet-hits (make-bullet 1 13 1) (list (make-alien-ship (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 1) 1 1) (make-alien-ship 1 13 1 1)))
              (list (make-alien-ship (+ HIT-RANGE-X 1) (+ HIT-RANGE-Y 1) 1 1)))

; (define (bullet-hits blt loe) loe) ; stub

(define (bullet-hits blt loe)
  (cond [(empty? loe) (cons blt empty)]
        [else
         (if (and (alien-ship? (first loe)) (bullet-touches-alien-ship? blt (first loe)))
             (rest loe)
             (cons (first loe) (bullet-hits blt (rest loe))))]))

;; bullet alien-ship -> Boolean
;; Tells if bullet has touched an alien ship
(define (bullet-touches-alien-ship? blt as)
  (and (> HIT-RANGE-X (abs (- (alien-ship-x as) (bullet-x blt))))
       (> HIT-RANGE-Y (abs (- (alien-ship-y as) (bullet-y blt))))))


;; ListOfEvents -> Boolean
;; Makes new alien ship appear once in three times.
(define (alien-ship-arrives? loe)
  (and (> 4 (count loe alien-ship?)) (= 1 (random 10))))


;; Integer -> Interval[0, MTS-MAX-WIDHT]
;; Creates an alien-ship at a random position on x axis.
(check-random (create-alien-ship ALIEN-SHIP-DX ALIEN-SHIP-DY) (make-alien-ship (random MTS-WIDTH) 0 ALIEN-SHIP-DX ALIEN-SHIP-DY))

(define (create-alien-ship dx dy)
  (make-alien-ship (random MTS-WIDTH) 0 dx dy))


;; Next Events
;; ListOfEvents -> ListOfEvents
;; Produces next iteration of ListOfEvents.

(check-expect (next-loe LOE1) (list (next-tank (first LOE1))))
(check-expect (next-loe LOE2) (list (next-tank (first LOE2)) (next-bullet (second LOE2))  (next-bullet (third LOE2))))
(check-expect (next-loe LOE3) (cons (next-alien-ship (first LOE3)) (cons (next-alien-ship (second LOE3)) (next-loe LOE2))))
(check-expect (next-loe (cons (make-bullet 12 0 BULLET-SPEED) LOE3)) (cons (next-alien-ship (first LOE3)) (cons (next-alien-ship (second LOE3)) (next-loe LOE2))))

; (define (next-loe loe) loe) ; stub
(define (next-loe loe)
  (cond [(empty? loe) empty]
        [else
         (if (empty? (next-event (first loe)))
             (next-loe (rest loe))
             (cons (next-event (first loe))
                   (next-loe (rest loe))))]))

;; Event -> Event
;; Calls appropriate next function for the event type.

;; Tests are handled in the caller.

; (define (next-event evnt) evnt) ; stub
(define (next-event evt)
  (cond [(tank? evt) (next-tank evt)]
        [(bullet? evt) (next-bullet evt)]
        [(alien-ship? evt) (next-alien-ship evt)]))

;; bullet -> bullet
;; Produces next state of the bullet.
(check-expect (next-bullet (make-bullet 1 12 BULLET-SPEED)) (make-bullet 1 (+ 12 BULLET-SPEED) BULLET-SPEED))
(check-expect (next-bullet (make-bullet 1 0 BULLET-SPEED)) empty) ; Bullet travels outside of canvas

; (define (next-bullet blt) blt) ; stub

(define (next-bullet blt)
  (cond [(> 0 (+ (bullet-y blt) (bullet-dy blt))) empty]
        [else
         (make-bullet (bullet-x blt) (+ (bullet-y blt) (bullet-dy blt)) (bullet-dy blt))]))


;; alien-ship -> alien-ship
;; Produces next state of the ship.

(check-expect (next-alien-ship (make-alien-ship 10 12 10 2)) (make-alien-ship 20 14 10 2))
(check-expect (next-alien-ship (make-alien-ship MTS-WIDTH 12 10 2)) (make-alien-ship (- MTS-WIDTH 10) 14 -10 2))
(check-expect (next-alien-ship (make-alien-ship 0 12 10 2)) (make-alien-ship 10 14 10 2))
(check-expect (next-alien-ship (make-alien-ship 0 MTS-HEIGHT 10 2)) empty)


; (define (next-alien-ship as) as) ; stub
(define (next-alien-ship as)
  (cond
    [(> (+ (alien-ship-y as) (alien-ship-dy as)) MTS-HEIGHT) empty]
    [(or (>= (+ (alien-ship-x as) (alien-ship-dx as)) MTS-WIDTH)
         (<= (+ (alien-ship-x as) (alien-ship-dx as)) 0))
     (make-alien-ship (+ (alien-ship-x as) (* -1 (alien-ship-dx as))) (+ (alien-ship-y as) (alien-ship-dy as)) (* -1 (alien-ship-dx as)) (alien-ship-dy as))]
    [else
     (make-alien-ship (+ (alien-ship-x as) (alien-ship-dx as))
                      (+ (alien-ship-y as) (alien-ship-dy as))
                      (alien-ship-dx as)
                      (alien-ship-dy as))]))


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


;; Renders
;; ListOfEvents -> Image
;; Renders the action from the list of events.

; (define (render-loe loe) MTS) ; stub

(define (render-loe loe)
  (cond [(empty? loe) MTS]
        [else
         (render-event (first loe) (render-loe (rest loe)))]))

;; Event Image -> Image
;; Calls correct event rederer.
; (define (render-event evt img) img) ; stub

(define (render-event evt img)
  (cond [(tank? evt) (render-tank evt img)]
        [(bullet? evt) (render-bullet evt img)]
        [(alien-ship? evt) (render-alien-ship evt img)]
        [(game-over? evt)  (render-game-over evt img)]))

;; Event Image -> Image
;; Draws a bullet on the Img
(check-expect (render-bullet (make-bullet 19 12 12) MTS) (place-image BULLET-SPRITE 19 12 MTS))

; (define (render-bullet evt img) img) ; stub
(define (render-bullet evt img)
  (place-image BULLET-SPRITE (bullet-x evt) (bullet-y evt) img))


;; Event Image -> Event
;; purp. It places an alien ship on the canvas.

(check-expect (render-alien-ship (make-alien-ship 1 1 12 12) MTS) (place-image ALIEN-SHIP-SPRITE 1 1 MTS))
(check-expect (render-alien-ship (make-alien-ship 11 13 12 12) MTS) (place-image ALIEN-SHIP-SPRITE 11 13 MTS))

; (define (render-alien-ship evt img) img) ; stub
(define (render-alien-ship evt img)
  (place-image ALIEN-SHIP-SPRITE
               (alien-ship-x evt)
               (alien-ship-y evt)
               img))


;; Event Image -> Event
;; purp. It renders GameOver.

(check-expect (render-game-over GO MTS) (place-image (text (game-over-text GO) 23 "Red") (/ MTS-WIDTH 2) (/ MTS-HEIGHT 2) MTS))

; (define (render-game-over evt img) img) ; stub
(define (render-game-over evt img)
  (place-image (text (game-over-text evt) 23 "Red")
               (/ MTS-WIDTH 2)
               (/ MTS-HEIGHT 2)
               img))


;; ListOfEvents -> ListOfEvents
;; Calls appropriate handle funciton depenending on the event type.

;; Handlers
; (define (handle-loe loe evt) loe) ; stub
(define (handle-loe loe evt)
  (cond [(empty? loe) empty]
        [else
         (join-lists (handle-event (first loe) evt) ; Event -> LiestOfEvents
                     (handle-loe (rest loe) evt))]))

;; List List -> List
;; Joints two lists into one flat list.

(check-expect (join-lists empty empty) empty)
(check-expect (join-lists 1 empty) (cons 1 empty))
(check-expect (join-lists (list 1 2 3) empty) (list 1 2 3))
(check-expect (join-lists (list 1 2 3) (list 1 23 4)) (list 1 2 3 1 23 4))

; (define (join-lists l1 l2) l1) ; stub

(define (join-lists l1 l2)
  (cond [(empty? l1) l2]
        [(list? l1)
         (cons
          (first l1)
          (join-lists (rest l1) l2))]
        [else
         (cons l1 l2)]))

;; ListOfEvent

;; Event KeyEvent -> ListOfEvents
;; Changes the state of the event depending on the key pressed.

; (define (handle-event evt) evt) ; stub
(define (handle-event evt key)
  (cond [(tank? evt) (handle-tank evt key)]
        [else evt]))

;; Tank Image -> Image
;; Renders tank from the tank data definition and places it on Image.

(check-expect (render-tank (make-tank 0 0 10) MTS) (place-image TANK-SPRITE 0 0 MTS))
(check-expect (render-tank (make-tank 10 0 10) MTS) (place-image TANK-SPRITE 10 0 MTS))

; (define (render-tank tank) MTS) ; stub
(define (render-tank tnk img)
  (place-image TANK-SPRITE (tank-x tnk) (tank-y tnk) img))

;; Tank KeyEvent -> Tank
;; Changes direction of the tank depending on Key pressed.

(check-expect (handle-tank (make-tank 0 12 10) "left")   (make-tank 0 12 -10))
(check-expect (handle-tank (make-tank 0 12 -10) "left")  (make-tank 0 12 -10))
(check-expect (handle-tank (make-tank 0 12 10) "right")  (make-tank 0 12 10))
(check-expect (handle-tank (make-tank 0 12 -10) "right") (make-tank 0 12 10))

(check-expect (handle-tank (make-tank 12 12 -10) " ") (list (make-tank 12 12 -10) (make-bullet 12 (- 12 (/ (image-height TANK-SPRITE) 2)) BULLET-SPEED)))


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
        [(key=? ke " ") (list tnk (make-bullet (tank-x tnk)  (- (tank-y tnk) (/ (image-height TANK-SPRITE) 2)) BULLET-SPEED))]
        [else tnk]))


;; Utilities

;; List -> Natural
;; Count number of elements in the list.

(check-expect (count (list 1 2 3 "a") number?) 3)
(check-expect (count (list 1 2 3 "a") string?) 1)

; (define (count lst predicate) lst) ; stub

(define (count lst predicate?)
  (cond [(empty? lst) 0]
        [else
         (if (predicate? (first lst))
             (+ 1 (count (rest lst) predicate?))
             (count (rest lst) predicate?))]))



(main (list (make-tank (/ MTS-WIDTH 2) (- MTS-HEIGHT (/ TANK-HEIGHT 2)) 10)))