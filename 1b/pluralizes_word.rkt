;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pluralizes_word) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;; String -> String
;;; produce puralizes a given word
;(define (puralize word) 0) ; the stub

(check-expect (puralize "rabbit") "rabbits") ; test
(check-expect (puralize "cat") "cats") ; test

;(define (puralize word) ; template
;  (... word))

(define (puralize word) 
  (string-append word "s"))