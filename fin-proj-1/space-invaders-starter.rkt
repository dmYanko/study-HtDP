;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)
(define MAX-INVADERS 10)

(define BATTLEFIELD (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:

(define-struct game (invaders missiles tank ticks))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))
       (fn-for-tank (game-ticks s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 0))
(define G2 (make-game (list I1) (list M1) T1 0))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 0))


;; ListOfInvaders is one of:
;;  - empty
;;  - (cons Invaders ListOfInvaders)
;; interp. list of invaders

(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I2 I3))

(define (fn-for-loi loi)
  (cond ((empty? loi) ...)
        (else
         (...
          (first loi)
          (fn-for-loi (rest loi))))))

;; Templates rules used:
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvaders)
;;  - self-reference: (rest loi) is ListOfInvaders


;; ListOfMissiles is one of:
;;  - empty
;;  - (cons missile ListOfMissiles)
;; interp. list of invaders

(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M2 M3))

(define (fn-for-lom lom)
  (cond ((empty? lom) ...)
        (else
         (...
          (first lom)
          (fn-for-lom (rest lom))))))

;; Templates rules used:
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvaders)
;;  - self-reference: (rest lom) is ListOfInvaders

(define-struct interval (x1 x2))
;; Interval is (make-interval Integer Integer)
;; interp. is interval started at x1 and ended at x2 (x1 < x2)

(define INTVL1 (make-interval 0 15))
(define INTVL2 (make-interval -2 19))
(define INTVL3 (make-interval -5 -1))

(define (fn-for-interval i)
  (... (interval-x1 i)
       (interval-x2 i)))

;; Template rules used:
;; - compound:
;; - atomic non-distinct: Integer
;; - atomic non-distinct: Integer


;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main g)
  (big-bang g                  ; 
    (on-tick  advance-game)    ; Game -> Game
    (to-draw  render-game)     ; Game -> Image
    (stop-when game-over?)     ; Game -> Boolean
    (on-key   handle-key)))   ; Game KeyEvent -> Game

;; Game -> Boolean
;; produce true if a game-over


(define (game-over? g)
  (invaders-win? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; produce true if one of invaders cross bottom corner

(check-expect (invaders-win? '()) false)
(check-expect (invaders-win? (list (make-invader 12 45 2))) false)
(check-expect (invaders-win? (list (make-invader 12 (+ HEIGHT 1) 2))) true)

;(define (invaders-win? loi) false)

(define (invaders-win? loi)
  (cond ((empty? loi) false)
        (else
         (if (> (invader-y (first loi)) (- HEIGHT 5) )
             true
             (invaders-win? (rest loi))))))


;; Game -> Game
;; produce the next game state:
;; - advance missiles, tank and invaders
;; - explose hitten targets
;; - delete missed missiles

(define (advance-game g)
  (clean-outside
   (hit-invaders (append (gen-invasion (game-ticks g))
                         (advance-invaders (game-invaders g)))
                 '()
                 (advance-missiles (game-missiles g))
                 (advance-tank (game-tank g))
                 (advance-ticks (game-ticks g)))))

;; Game -> Game
;; clear game objects placed outside of battlefield

(define (clean-outside g)
  (make-game (clean-invaders-outside (game-invaders g))
             (clean-missiles-outside (game-missiles g))
             (game-tank g)
             (game-ticks g)))


;; ListOfInvaders -> ListOfInvaders
;; clear invaders placed outside of battlefield
(check-expect (clean-invaders-outside empty) empty)
(check-expect (clean-invaders-outside (list (make-invader 0 25 1))) (list (make-invader 0 25 1))) ; inside
(check-expect (clean-invaders-outside (list (make-invader 0 25 1) (make-invader 0 -1 1)))
              (list (make-invader 0 25 1))); outside top corner
(check-expect (clean-invaders-outside (list (make-invader 0 25 1) (make-invader 0 (+ HEIGHT 1) 1)))
              (list (make-invader 0 25 1))); outside bottom corner
(check-expect (clean-invaders-outside (list (make-invader 35 25 1) (make-invader -1 0 1)))
              (list (make-invader 35 25 1))); outside left corner
(check-expect (clean-invaders-outside (list (make-invader 35 25 1) (make-invader (+ WIDTH 1) 0 1)))
              (list (make-invader 35 25 1))); outside right corner


;(define (clean-invaders-outside loi) empty)

;; <Template used from ListOfInvaders>

(define (clean-invaders-outside loi)
  (cond ((empty? loi) empty)
        (else
         (if (outside-battlefield? (invader-x (first loi))
                                   (invader-y (first loi)))
             (clean-invaders-outside (rest loi))
             (cons (first loi)
                   (clean-invaders-outside (rest loi)))))))
  

;; ListOfMissiles -> ListOfMissiles
;; clear missiles placed outside of battlefield
(check-expect (clean-missiles-outside empty) empty)
(check-expect (clean-missiles-outside (list (make-missile 0 25))) (list (make-missile 0 25))) ; inside
(check-expect (clean-missiles-outside (list (make-missile 0 25) (make-missile 0 -1)))
              (list (make-missile 0 25))); outside top corner
(check-expect (clean-missiles-outside (list (make-missile 0 25) (make-missile 0 (+ HEIGHT 1))))
              (list (make-missile 0 25))); outside bottom corner
(check-expect (clean-missiles-outside (list (make-missile 35 25) (make-missile -1 0)))
              (list (make-missile 35 25))); outside left corner
(check-expect (clean-missiles-outside (list (make-missile 35 25) (make-missile (+ WIDTH 1) 0)))
              (list (make-missile 35 25))); outside right corner

;(define (clean-missiles-outside lom) empty)

;; <Template rules used from ListOfMissiles>

(define (clean-missiles-outside lom)
  (cond ((empty? lom) empty)
        (else
         (if (outside-battlefield? (missile-x (first lom))
                                   (missile-y (first lom)))
             (clean-missiles-outside (rest lom))
             (cons (first lom)
                   (clean-missiles-outside (rest lom)))))))
  

;; Number, Number -> Boolean
;; produce true if given position outside of battlefield

(check-expect (outside-battlefield? 0 0) false)
(check-expect (outside-battlefield? WIDTH HEIGHT) false)
(check-expect (outside-battlefield? (+ WIDTH 1) HEIGHT) true)
(check-expect (outside-battlefield? WIDTH (+ HEIGHT 1)) true)
(check-expect (outside-battlefield? (+ WIDTH 22) (+ HEIGHT 20)) true)
(check-expect (outside-battlefield? -1 0) true)
(check-expect (outside-battlefield? 0 -1) true)
(check-expect (outside-battlefield? -30 -5) true)

;(define (outside-battlefield? x y) true)

#;
(define (outside-battlefield? x y)
  (... x y))

(define (outside-battlefield? x y)
  (or (not (and (>= x 0) (<= x WIDTH)))
      (not (and (>= y 0) (<= y HEIGHT)))))


;; ListOfInvaders, ListOfMissiles, ListOfMissiles, Tank -> Game
;; removes from game invaders and missiles which hit each over 


(check-expect (hit-invaders empty empty empty (make-tank 0 1) 0)
              (make-game empty empty (make-tank 0 1) 0))

(check-expect (hit-invaders empty
                            empty
                            (list (make-missile 25 25))
                            (make-tank 0 1)
                            0)
              (make-game empty
                         (list (make-missile 25 25))
                         (make-tank 0 1) 0))

(check-expect (hit-invaders (list (make-invader 25 25 INVADER-X-SPEED))
                            empty
                            (list (make-missile 25 25))
                            (make-tank 0 1)
                            0)
              (make-game empty
                         empty
                         (make-tank 0 1) 0))

#;
(define (hit-invaders loi missed lom tank ticks)
  (make-game loi lom tank ticks))

;; <Template used from ListOfMissiles>


(define (hit-invaders loi missed lom tank ticks)
  (cond ((empty? lom) (make-game loi missed tank ticks))
        (else
         (let ((survived (hit (first lom) loi)))
           (if (equal? survived loi)
               (hit-invaders survived
                             (cons (first lom) missed)
                             (rest lom)
                             tank
                             ticks)
               (hit-invaders survived
                             missed
                             (rest lom)
                             tank
                             ticks))))))
             

;; Missle, ListOfInvaders -> ListOfInvaders
;; remove wrecked invaders from list

(check-expect (hit (make-missile 10 10) empty)
              empty)
(check-expect (hit (make-missile 10 10)
                   (list (make-invader 10 10 1)))
              empty)
(check-expect (hit (make-missile 10 10)
                   (list (make-invader 100 10 1)))
              (list (make-invader 100 10 1)))
(check-expect (hit (make-missile 10 10)
                   (list (make-invader 100 10 1) (make-invader 20 10 1)))
              (list (make-invader 100 10 1)))


;(define (hit m loi) empty)

;; <Template used from ListOfInvaders>

(define (hit m loi)
  (cond ((empty? loi) empty)
        (else
         (if (collision? (first loi) m)
             (hit m (rest loi))
             (cons (first loi)
                   (hit m (rest loi)))))))


;; Invader, Missle -> Boolean
;; produce true if invader has collision with any missile

(check-expect (collision? (make-invader 11 11 1) (make-missile 1 1)) true) ; 
(check-expect (collision? (make-invader 11 11 1) (make-missile 0 0)) false)
(check-expect (collision? (make-invader 10 10 1) (make-missile 10 25)) true) ;
(check-expect (collision? (make-invader 10 10 1) (make-missile 10 50)) false) ;

(check-expect (collision? (make-invader 10 10 1) (make-missile 10 26)) true) ;
(check-expect (collision? (make-invader 10 10 1) (make-missile 10 27)) false) ;

;(define (collision? i m) true)

(define (collision? i m)
  (and (intersection? (make-interval (invader-x i)
                                     (+ (invader-x i) (image-width INVADER)))
                      (make-interval (missile-x m)
                                     (+ (missile-x m) HIT-RANGE)))
       (intersection? (make-interval (invader-y i)
                                     (+ (invader-y i) (image-height INVADER)))
                      (make-interval (missile-y m)
                                     (+ (missile-y m) HIT-RANGE)))))


;; Interval, Interval -> Boolean
;; produce true if i1 intersect with i2

(check-expect (intersection? (make-interval 0 3)
                             (make-interval 4 6))
              false) ; i1.x1 < i2.x1, i1.x2 < i2.x1

(check-expect (intersection? (make-interval 0 4)
                             (make-interval 4 6))
              true) ; i1.x1 < i2.x1, i1.x2 = i2.x1
(check-expect (intersection? (make-interval 0 4)
                             (make-interval 3 6))
              true) ; i1.x1 < i2.x1, i1.x2 > i2.x1
(check-expect (intersection? (make-interval 0 4)
                             (make-interval 0 6))
              true) ; i1.x1 = i2.x1, i1.x2 > i2.x1
(check-expect (intersection? (make-interval 4 5)
                             (make-interval 2 3))
              false) ; i1.x1 > i2.x1, i1.x2 > i2.x2
(check-expect (intersection? (make-interval 4 5)
                             (make-interval 2 6))
              true) ; i1.x1 > i2.x1, i1.x2 < i2.x2

(check-expect (intersection? (make-interval 4 5)
                             (make-interval 2 5))
              true) ; i1.x1 > i2.x1, i1.x2 = i2.x2

;(define (intersection? i1 i2) true)

#;
(define (intersection? i1 i2)
  (... (interval-x1 i1)
       (interval-x2 i1)
       (interval-x1 i2)
       (interval-x2 i2)))

(define (intersection? i1 i2)
  (cond ((and (<= (interval-x1 i1)
                  (interval-x1 i2))
              (>= (interval-x2 i1)
                  (interval-x1 i2)))
         true)
        ((and (> (interval-x1 i1)
                 (interval-x1 i2))
              (<= (interval-x1 i1)
                  (interval-x2 i2)))
         true)
        (else
         false)))
      

;; Number -> ListOfInvaders
;; generate invaders 

(define (gen-invasion rate)
  (if (= (remainder rate INVADE-RATE) 0)
      (gen-new-invaders (random MAX-INVADERS))
      '()))
   

;; Number -> ListOfInvaders
;; generate few new invaders depends on given number

(check-random (gen-new-invaders 0) empty)
(check-random (gen-new-invaders 1) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)))
(check-random (gen-new-invaders 2) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                                         (make-invader (random WIDTH) 0 INVADER-X-SPEED)))

;(define (gen-new-invaders n) empty)

;; <template used from ListOfInvaders>

(define (gen-new-invaders n)
  (cond ((zero? n) empty)
        (else
         (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
               (gen-new-invaders (- n 1))))))



;; ListOfInvaders -> ListOfInvaders
;; advance x and y position of the invaders forward

(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 10 10 1)))
              (list (make-invader 11 (+ 10 INVADER-Y-SPEED) 1)))
(check-expect (advance-invaders (list (make-invader 10 10 1)
                                      (make-invader 25 30 (- 1))))
              (list (make-invader 11 (+ 10 INVADER-Y-SPEED) 1)
                    (make-invader 24 (+ 30 INVADER-Y-SPEED) (- 1))))

;(define (advance-invaders loi) empty)
  
;; <template used from ListOfInvaders>

(define (advance-invaders loi)
  (cond ((empty? loi) empty)
        (else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi))))))


;; Invader -> Invader
;; advance invader increase x position by dx,
;; y position by INVADER-Y-SPEED

(check-expect (advance-invader (make-invader 0 10 INVADER-X-SPEED))
              (make-invader (+ 0 INVADER-X-SPEED)
                            (+ 10 INVADER-Y-SPEED)
                            INVADER-X-SPEED)) ; left-to-right direction
(check-expect (advance-invader (make-invader 20 10 (- INVADER-X-SPEED)))
              (make-invader (- 20 INVADER-X-SPEED)
                            (+ 10 INVADER-Y-SPEED)
                            (- INVADER-X-SPEED))) ; right-to-left direction
(check-expect (advance-invader (make-invader 0 10 (- INVADER-X-SPEED)))
              (make-invader 0
                            (+ 10 INVADER-Y-SPEED)
                            INVADER-X-SPEED)) ; change direction to left-to-right
(check-expect (advance-invader (make-invader INVADER-X-SPEED 10 (- INVADER-X-SPEED)))
              (make-invader 0
                            (+ 10 INVADER-Y-SPEED)
                            INVADER-X-SPEED)) ; change direction to left-to-right
(check-expect (advance-invader (make-invader WIDTH 10 INVADER-X-SPEED))
              (make-invader WIDTH
                            (+ 10 INVADER-Y-SPEED)
                            (- INVADER-X-SPEED))) ; change direction to right-to-left
(check-expect (advance-invader (make-invader (- WIDTH INVADER-X-SPEED) 10 INVADER-X-SPEED))
              (make-invader WIDTH
                            (+ 10 INVADER-Y-SPEED)
                            (- INVADER-X-SPEED))) ; change direction to right-to-left

;(define (advance-invader i) empty)
;; <template used from invader>

(define (advance-invader i)
  (cond ((<= (+ (invader-x i) (invader-dx i)) 0)
         (make-invader 0
                       (+ (invader-y i) INVADER-Y-SPEED)
                       INVADER-X-SPEED))
        ((>= (+ (invader-x i) (invader-dx i)) WIDTH)
         (make-invader WIDTH
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- INVADER-X-SPEED)))
        (else
         (make-invader (+ (invader-x i) (invader-dx i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i)))))


;; ListOfMissiles -> ListOfMissiles
;; advance y position of the missile
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 10 10)))
              (list (make-missile 10 (- 10 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 10 10)
                                      (make-missile 20 30)))
              (list (make-missile 10 (- 10 MISSILE-SPEED))
                    (make-missile 20 (- 30 MISSILE-SPEED))))

;(define (advance-missiles loi) empty)
;; <template used from ListOf-Missles>

(define (advance-missiles lom)
  (cond ((empty? lom) empty)
        (else
         (cons (advance-missile (first lom))
               (advance-missiles (rest lom))))))

;; Missle -> Missle
;; increase y-coord of the missle by misssle speed

(check-expect (advance-missile (make-missile 10 10))
              (make-missile 10 (- 10 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile (- 10) 10))
              (make-missile (- 10) (- 10 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 10 (- 10)))
              (make-missile 10 (- (- 10) MISSILE-SPEED)))

;(define (advance-missile m) empty)

;; <template used from Missile>

(define (advance-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; advance x position of the tank

(check-expect (advance-tank (make-tank 0 (- TANK-SPEED)))
              (make-tank 0 (- TANK-SPEED)))
(check-expect (advance-tank (make-tank WIDTH TANK-SPEED))
              (make-tank WIDTH TANK-SPEED))
(check-expect (advance-tank (make-tank 1 (- TANK-SPEED)))
              (make-tank 0 (- TANK-SPEED)))
(check-expect (advance-tank (make-tank (- WIDTH 1) TANK-SPEED))
              (make-tank WIDTH TANK-SPEED))
(check-expect (advance-tank (make-tank WIDTH (- TANK-SPEED)))
              (make-tank (- WIDTH TANK-SPEED) (- TANK-SPEED)))
(check-expect (advance-tank (make-tank 0 TANK-SPEED))
              (make-tank (+ 0 TANK-SPEED) TANK-SPEED))

;(define (advance-tank t) empty)

;; <template used from Tank>

(define (advance-tank t)
  (cond ((<= (+ (tank-x t) (tank-dir t)) 0)
         (make-tank 0 (- TANK-SPEED)))        
        ((>= (+ (tank-x t) (tank-dir t)) WIDTH)
         (make-tank WIDTH TANK-SPEED))
        (else
         (make-tank (+ (tank-x t)
                       (tank-dir t))
                    (tank-dir t)))))

;; Number -> Number
;; increase count on each timer tick

(check-expect (advance-ticks 4)
              5)


;(define (advance-ticks g) 0)

;; <template used from Game>

(define (advance-ticks n)
  (+ n 1))



;; Game -> Image
;; render game

(define (render-game g)
  (render-tank (game-tank g)
               (render-invaders (game-invaders g)
                                (render-missiles (game-missiles g)
                                                 BATTLEFIELD))))
  
;; ListOfMissiles -> Image
;; render list of missiles


;(define (render-missiles lom scene) empty-scene)

(define (render-missiles lom scene)
  (cond ((empty? lom) scene)
        (else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) scene)))))


;; ListOfInvaders -> Image
;; render list of invaders


;(define (render-invaders loi scene) empty-scene)

(define (render-invaders loi scene)
  (cond ((empty? loi) scene)
        (else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) scene)))))


;; Tank -> Image
;; render tank

(check-expect (render-tank (make-tank 0 TANK-SPEED)
                           BATTLEFIELD)
              (place-image TANK 0 (- HEIGHT TANK-HEIGHT/2) BATTLEFIELD))

;(define (render-tank g) empty-scene)

;; <template used from Game>

(define (render-tank t scene)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               scene))
  

;; Game -> Game
;; change game on one of key event:
;; - "left" change tank direction to left
;; - "right" change tank direction to right
;; - " " add new missle to list of missles

(check-expect (handle-key empty "left") empty)
(check-expect (handle-key (make-game empty
                                     empty
                                     (make-tank 10 TANK-SPEED) 0)
                          "left")
              (make-game empty
                         empty
                         (make-tank 10 (- TANK-SPEED)) 0))
(check-expect (handle-key (make-game empty
                                     empty
                                     (make-tank 10 (- TANK-SPEED)) 0)
                          "left")
              (make-game empty
                         empty
                         (make-tank 10 (- TANK-SPEED)) 0))
(check-expect (handle-key (make-game empty
                                     empty
                                     (make-tank 10 (- TANK-SPEED)) 0)
                          "right")
              (make-game empty
                         empty
                         (make-tank 10 TANK-SPEED) 0))
(check-expect (handle-key (make-game empty
                                     empty
                                     (make-tank 10 TANK-SPEED) 0)
                          "right")
              (make-game empty
                         empty
                         (make-tank 10 TANK-SPEED) 0))
(check-expect (handle-key (make-game empty
                                     empty
                                     (make-tank 10 TANK-SPEED) 0)
                          " ")
              (make-game empty
                         (list (make-missile 10 HEIGHT))
                         (make-tank 10 TANK-SPEED) 0))
(check-expect (handle-key (make-game empty
                                     (list (make-missile 10 100))
                                     (make-tank 28 TANK-SPEED) 0)
                          " ")
              (make-game empty
                         (list (make-missile 28 HEIGHT)
                               (make-missile 10 100))                       
                         (make-tank 28 TANK-SPEED) 0))

;(define (handle-key g ke) empty)

#;
(define (handle-key g ke)
  (cond ((empty? g) empty)
        ((key=? ke " ") (... g))
        (else 
         (... g))))

(define (handle-key g ke)
  (cond ((empty? g) empty)
        ((key=? ke " ") (shoot-missile g))
        ((key=? ke "left") (turn-tank g (- TANK-SPEED)))
        ((key=? ke "right") (turn-tank g TANK-SPEED))
        (else g)))


;; Game -> Game
;; add new missile to list of missile at position (tank-x, HEIGHT)

(check-expect (shoot-missile (make-game empty
                                        empty
                                        (make-tank 20 TANK-SPEED) 0))
              (make-game empty
                         (list (make-missile 20 HEIGHT))
                         (make-tank 20 TANK-SPEED) 0))
(check-expect (shoot-missile (make-game empty
                                        (list (make-missile 34 67))
                                        (make-tank 50 TANK-SPEED) 0))
              (make-game empty
                         (list (make-missile 50 HEIGHT)
                               (make-missile 34 67))
                         (make-tank 50 TANK-SPEED) 0))                             

;(define (shoot-missile g) empty)

;; <template used from Game>

(define (shoot-missile g)
  (make-game (game-invaders g)
             (cons (make-missile (tank-x (game-tank g))
                                 HEIGHT)
                   (game-missiles g))
             (game-tank g)
             (game-ticks g)))

;; Game -> Game
;; change tank direction to dir

(check-expect (turn-tank (make-game empty
                                    empty
                                    (make-tank 10 (- TANK-SPEED)) 0)
                         (- TANK-SPEED))
              (make-game empty
                         empty
                         (make-tank 10 (- TANK-SPEED)) 0))

(check-expect (turn-tank (make-game empty
                                    empty
                                    (make-tank 10 TANK-SPEED) 0)
                         (- TANK-SPEED))
              (make-game empty
                         empty
                         (make-tank 10 (- TANK-SPEED)) 0))

(check-expect (turn-tank (make-game empty
                                    empty
                                    (make-tank 10 TANK-SPEED) 0)
                         TANK-SPEED)
              (make-game empty
                         empty
                         (make-tank 10 TANK-SPEED) 0))

(check-expect (turn-tank (make-game empty
                                    empty
                                    (make-tank 10 (- TANK-SPEED)) 0)
                         TANK-SPEED)
              (make-game empty
                         empty
                         (make-tank 10 TANK-SPEED) 0))


;(define (turn-tank g dir) empty)

;; <template used from Game>

(define (turn-tank g dir)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) dir)
             (game-ticks g)))


(main (make-game (list (make-invader 0 0 INVADER-X-SPEED)
                       (make-invader 100 0 INVADER-X-SPEED)
                       (make-invader 100 0 (- INVADER-X-SPEED))) empty (make-tank 0 0) 0))




