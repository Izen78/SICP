#lang sicp

; Exercise 2.1
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> (* n d) 0)
        (cons (/ n g) (/ d g))
        (cons (- (/ n g)) (- (/ d g))))))

; Exercise 2.2
(define (average x y)
  (/ (+ x y) 2))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((x-start (car (start-segment seg)))
        (x-end (cdr (start-segment seg)))
        (y-start (car (end-segment seg)))
        (y-end (cdr (end-segment seg))))
    (make-point (average x-start y-start) (average x-end y-end))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Exercise 2.3
(define (rect p1 p2 p3 p4) ; clockwise from bottom left -- requires assumption on points being parallel to axis, another representation would be with segments but I'm not bothered
  (let ((left-side (cons p1 p2))
        (right-side (cons p3 p4)))
    ((cons left-side right-side))))
(define (rect-p1 rect)
  (car (car rect)))
(define (rect-p2 rect)
  (cdr (car rect)))
(define (rect-p3 rect)
  (car (cdr rect)))
(define (rect-p4 rect)
  (cdr (cdr rect)))

(define (perim rect)
  (* 2 (+ (- (y-point (rect-p1 rect)) (y-point (rect-p2 rect))) (+ (- (x-point (rect-p3 rect)) (x-point (rect-p4 rect)))))))
(define (area rect)
  (* (y-point (rect-p1 rect)) (y-point (rect-p2 rect))) (- (x-point (rect-p3 rect)) (x-point (rect-p4 rect))))

; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

; Exercise 2.5
(define (power x y)
  (if (= y 0)
      1
      (* x (power x (- y 1)))))
(define (cons-n a b)
  (* (power 2 a) (power 3 b)))

(define (car-n num)
  (define (iter i result)
    (if (= (remainder result 2) 0)
        (iter (+ i 1) (/ result 2))
        i))
  (iter 0 num))

(define (cdr-n num)
  (define (iter i result)
    (if (= (remainder result 3) 0)
        (iter (+ i 1) (/ result 3))
        i))
  (iter 0 num))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
; One
(define one (lambda (f) (lambda (x) (f x))))
; Two
(define two (lambda (f) (lambda (x) (f (f x)))))
; Addition - Incomplete

; Exercise 2.7