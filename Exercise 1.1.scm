#lang sicp
; Exercise 1.1

;10

;(+ 5 3 4)

;(- 9 1)

;(/ 6 2)

;(+ (* 2 4) (- 4 6))

;(define a 3)

;(define b (+ a 1))

;(+ a b (* a b))

;(= a b)

;(if (and (> b a) (< b (* a b))) b a)

;(cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;      (else 25))

;(+ 2 (if (> b a) b a))

;(* (cond ((> a b) a)
;         ((< a b) b)
;         (else -1))
;   (+ a 1))

; Exercise 1.2

;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3

(define (sum-of-square x y) (+ (* x x) (* y y)))

(define (two-largest x y z)
  (if (and (< x y) (< x z))
      (sum-of-square y z) ; y and z
      (if (< y z)
          (sum-of-square x z) ; x and z
          (sum-of-square x y)))) ; x and y

;(two-largest 1 2 3)
;(two-largest 2 1 3)
;(two-largest 3 1 2)

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

;(test 0 (p))

; Applicative Order:
; Infinite loop

; Normal Order:
; Returns 0

; Exercise 1.6

; square root by Newton's Method
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 9)
(define (new-if predicate then-clause else-clause) ; Arguments: predicate, then-clause, else-clause
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-if (improve guess x)
                     x)))
(define (sqrt-if x) (sqrt-iter-if 1.0 x))

; (sqrt-if 9)
; Infinite Loop
; Scheme uses applicative-order evaluation and since new-if is a procedure (NOT a special form) it evaluates all arguments before implementing the operator
; The arguments are 'guess' and '(sqrt-iter-if (improve guess...' Thus, the latter will recurse infinitely.
; ie. The predicate and two cases will evaluate no matter what the value of the predicate is

; Exercise 1.7

; Inaccurate answers since real numbers can't be represented fully using a finite number of bits

; Wrong Answer
;(sqrt_ 0.000000000123)
;(square 0.0312500013107187390371485)

; Doesn't finish 
;(sqrt_ 123123123123123123.123)
(define (good-enough?two guess x)
  (< (/ (abs (- guess (improve guess x))) (guess)) 0.001))

; Exercise 1.8
(define (improve-cb y x) ; y is the guess
  (/ (+ (/ x (square y)) (* 2 y)) 3))
(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve-cb guess x) x)))

(define (cbrt x) (cbrt-iter 1.0 x))