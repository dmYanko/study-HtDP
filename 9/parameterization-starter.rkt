;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parameterization-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; parameterization-starter.rkt
(define (area r) (* pi (sqr r)))

(area (* pi (sqr 4))) ;area of circle radius 4
(area (* pi (sqr 6)));area of circle radius 6


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"

(define (contains-ubc? los) (contains? "UBC" los))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"

(define (contains-mcgill? los) (contains? "McGill" los))

;; String ListOfString -> Boolean
;; produce true if los includes s

(check-expect (contains? "McGill" empty) false)
(check-expect (contains? "McGill" (cons "UBC" empty)) false)
(check-expect (contains? "McGill" (cons "McGill" empty)) true)
(check-expect (contains? "McGill" (cons "UBC" (cons "McGill" empty))) true)

(define (contains? s los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             true
             (contains? s (rest los)))]))


;; ====================

;; (listof Number) -> (listof Number)
;; produce list of sqr of every number in lon

(define (squares lon) (map2 sqr lon))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon

(define (square-roots lon) (map2 sqrt lon))

;; (X -> Y) (listof X) -> (listof Y)
;; aplly given function to every number in lon

(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqrt (list 9 16)) (list 3 4))
(check-expect (map2 sqr (list 3 4)) (list 9 16))
(check-expect (map2 string-length (list "a" "bc" "efg")) (list 1 2 3))

(define (map2 f lon)
  (cond [(empty? lon) empty]
        [else
         (cons (f (first lon))
               (map2 f (rest lon)))]))


;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon

(define (positive-only lon) (filter2 positive? lon))


;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon

(define (negative-only lon) (filter2 negative? lon))

;; (X -> Boolean) (listOf X) -> (listOf X)
;; produce list with only filtered by pred? elements of lon

(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 positive? (list 1 -2 3 -4)) (list 1 3))
(check-expect (filter2 negative? (list 1 -2 3 -4)) (list -2 -4))

(define (filter2 pred? lon)
  (cond [(empty? lon) empty]
        [else
         (if (pred? (first lon))
             (cons (first lon)
                   (filter2 pred? (rest lon)))
             (filter2 pred? (rest lon)))]))