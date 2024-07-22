;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-images-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; arrange-images-starter.rkt (problem statement)



;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. list of images


(define LOI0 empty)
(define LOI1 (cons (text "1" 20 "black") empty))
(define LOI2 (cons (text "1" 20 "black")
                   (cons (text "1" 20 "black")
                         empty)))

(define (func-for-loi loi)
  (cond ((empty? loi) ...)
        (else
         (... (first loi)
              (func-for-loi (rest loi))))))

;; Templates rules used:
;; is one of 2 cases:
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - self-reference: (rest los) is ListOfImage


;; ListOfImage -> ListOfImage
;; produce list of images in increasing order of size


(check-expect (arrange-images empty) empty-image)

(check-expect (arrange-images (cons (rectangle 10 30 "solid" "red") empty))
              (rectangle 10 30 "solid" "red"))

(check-expect (arrange-images (cons (rectangle 30 30 "solid" "blue")
                                    (cons (rectangle 20 30 "solid" "green")
                                          (cons (rectangle 10 30 "solid" "red") empty))))
              (beside (rectangle 10 30 "solid" "red")
                      (rectangle 20 30 "solid" "green")
                      (rectangle 30 30 "solid" "blue")))

;;(define (arrange-images loi) empty)

;; <template used from ListOfImage>

(define (arrange-images loi)
  (layout-images (sort-images loi)))
 

;; ListOfImage -> ListOfImage
;; produce sorted list of images by their sizes in increasing order

(check-expect (sort-images empty) empty)
(check-expect (sort-images (cons (rectangle 10 30 "solid" "red") empty))
              (cons (rectangle 10 30 "solid" "red") empty))
(check-expect (sort-images (cons (rectangle 30 30 "solid" "blue")
                                    (cons (rectangle 20 30 "solid" "green")
                                          (cons (rectangle 10 30 "solid" "red") empty))))
              (cons (rectangle 10 30 "solid" "red")
                    (cons (rectangle 20 30 "solid" "green")
                          (cons (rectangle 30 30 "solid" "blue") empty))))

;(define (sort-images loi) empty)

(define (sort-images loi)
  (cond ((empty? loi) empty)
        (else
         (insert (first loi)
                 (sort-images (rest loi))))))

;; Image, ListOfImage -> ListOfImage
;; insert image in sorted list

(check-expect (insert empty-image empty) (cons empty-image empty))
(check-expect (insert (rectangle 10 30 "solid" "red") empty) (cons (rectangle 10 30 "solid" "red")
                                                                   empty))

(check-expect (insert (rectangle 10 30 "solid" "red") (list (rectangle 5 30 "solid" "white")
                                                            (rectangle 20 30 "solid" "green")
                                                            (rectangle 30 30 "solid" "blue")))
                      (list (rectangle 5 30 "solid" "white")
                            (rectangle 10 30 "solid" "red")
                            (rectangle 20 30 "solid" "green")
                            (rectangle 30 30 "solid" "blue")))

(check-expect (insert (rectangle 5 30 "solid" "white") (list (rectangle 5 30 "solid" "white")
                                                            (rectangle 20 30 "solid" "green")
                                                            (rectangle 30 30 "solid" "blue")))
                      (list (rectangle 5 30 "solid" "white")
                            (rectangle 5 30 "solid" "white")
                            (rectangle 20 30 "solid" "green")
                            (rectangle 30 30 "solid" "blue")))


;(define (insert img loi) empty)

(define (insert img loi)
  (cond ((empty? loi) (cons img empty))
        (else
         (if (bigger-image? img (first loi))
             (cons (first loi) (insert img (rest loi)))
             (cons img loi)))))


;; Image, Image -> Boolean
;; produce true if image bigger than etalon image

(check-expect (bigger-image? empty-image empty-image) false)
(check-expect (bigger-image? (rectangle 10 30 "solid" "red") empty-image) true)
(check-expect (bigger-image? empty-image (rectangle 10 30 "solid" "red")) false)

;(define (bigger-image? img etln) true)

(define (bigger-image? img etln)
  (> (image-size img)
     (image-size etln)))

;; Image -> Natural
;; produce size of image

(check-expect (image-size empty-image) 0)
(check-expect (image-size (rectangle 10 30 "solid" "red")) 300)

;(define (image-size img) 0)

#;
(define (image-size img)
  (... img))

(define (image-size img) (* (image-height img)
                            (image-width img)))

;; ListOfImage -> Image
;; place images beside each over in normal list order
(check-expect (layout-images empty) empty-image)
(check-expect (layout-images (cons (rectangle 10 30 "solid" "red") empty))
              (rectangle 10 30 "solid" "red"))
(check-expect (layout-images (cons (rectangle 10 30 "solid" "red")
                                   (cons (rectangle 20 30 "solid" "green") empty)))
              (beside (rectangle 10 30 "solid" "red")
                      (rectangle 20 30 "solid" "green")))

;(define (layout-images loi) empty)
; <template used from loi>

(define (layout-images loi)
  (cond ((empty? loi) empty-image)
        (else
         (beside (first loi)
                 (layout-images (rest loi))))))