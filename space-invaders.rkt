;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
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
