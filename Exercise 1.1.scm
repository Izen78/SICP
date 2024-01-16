;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 1.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 1.1

10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b))) b a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3

(define (sum-of-square x y) (+ (* x x) (* y y)))

(define (two-largest x y z)
  (if (and (< x y) (< x z))
      (sum-of-square y z) ; y and z
      (if (< y z)
          (sum-of-square x z) ; x and z
          (sum-of-square x y)))) ; x and y

(two-largest 1 2 3)
(two-largest 2 1 3)
(two-largest 3 1 2)

; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; if b is greater  than 0 the operator chosen will be + otherwise, - operator used.

; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; Applicative Order:
; Infinite loop

; Normal Order:
; Returns 0

; Exercise 1.6