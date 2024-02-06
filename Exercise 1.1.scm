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

; (sqrt 9)
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


; Exercise 1.9
; First procedure - linear recursive process
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
; Second procedure - linear iterative process
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(A 1 10) ; 1024
;(A 2 4) ; 65536
;(A 3 3) ; 65536

(define (f n) (A 0 n)) ; computes 2n
(define (g n) (A 1 n)) ; returns 2^n
;(A 0 (A 1 (- n 1)))
;(A 0 (A 0 ... n times (A 1 (1)))
(define (h n) (A 2 3)) ; too long, answer is 2^(h(n-1))

; Exercise 1.11
; Recursive Process
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))
; Iterative Process
(define (f-iter a b c total) ; INCORRECT
  (if (= total 0)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- total 1))))
(define (fx n)
  (f-iter 2 1 0 n))

; Exercise 1.12 ; INCORRECT
(define (pascal-iter row col n)
  (cond ((= row n) n)
      ((or (= row 0) (= col row)) 1)
      (else (+ (pascal-iter (- row 1) (- col 1) (+ n 1)) (pascal-iter (- row 1) col (+ n 1))))))

; Exercise 1.16 ; forgot the odd part
(define (fast-exp a b n)
  (cond ((= n 0) a)
      ((= (remainder n 2) 0) (fast-exp a (square b) (/ n 2)))
      (else (fast-exp (* a b) b (- n 1)))))

; Exercise 1.17 ; added a 'c' instead of adding 'a' in 'else'
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (mult a b)
  (cond ((= b 1) a)
        ((= b 0) a)
        ((= (remainder b 2) 0) (mult (double a) (halve b)))
        (else (+ a(mult a (- b 1))))))

; Exercise 1.18
(define (mult-iter a b c)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((= (remainder b 2) 0) (mult-iter (double a) (halve b) c))
        (else (mult-iter (+ a c) (- b 1) c))))

(define (final-mult a b)
  (mult-iter a b a))

; Exercise 1.19 ; couldn't figure out math portion
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((= (remainder count 2) 0)
        (fib-iter a
                  b
                  (+ (square p) (square q))
                  (+ (* 2 p q) (square q))
                  (/ count 2)))
  (else (fib-iter (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- count 1)))))

; Exercise 1.20 ; Normal Order Incorrect - remainder hasn't been called only expanding, only called when in 'if' predicate expression 18 times
; Normal Order - Infinite remainder operations?
;(gcd 206 40)
;(gcd 40 (remainder 206 40)) ; Called here once for if statement predicate
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))

; Applicative Order - 4 remainder operations
;(gcd 206 40)
;(gcd 40 (remainder 206 40)) -> (gcd 40 6)
;(gcd 6 (remainder 40 6)) -> (gcd 6 4)
;(gcd 4 (remainder 6 4)) -> (gcd 4 2)
;(gcd 2 (remainder 4 2)) -> (gcd 2 0)

; Exercise 1.22
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (even? n)
  (= (remainder n 2) 0))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
           (cond ((and (not (= base 1)) (not (= base (- m 1))) (= (remainder (square (expmod base (/ exp 2) m))) 1)) 0)
         (else (remainder (square (expmod base (/ exp 2) m)) m))))
         (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))
      (display "not prime")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n max prime-count)
  (cond ((= prime-count 3) (display "finished"))
        ((> n max) (display "went over"))
        (else (timed-prime-test n)(search-for-primes (+ n 1) max (if (prime? n) (+ 1 prime-count) prime-count)))))

;(search-for-primes 1000 10000 0) ; 7, 10, 8 -> 8.33 ---- 1009, 1013,, 1019
;(search-for-primes 10000 100000 0) ; 17, 16, 16 -> 16.33 (should be 26) ---- 10007, 10009, 10037
;(search-for-primes 100000 1000000 0) ; 43, 43, 40 -> 42 (should be 51.6) ---- 100003, 100019, 100043
;(search-for-primes 1000000 1005000 0) ; 113, 129, 159 -> 133.67 (132.8) ---- 1000003, 1000033, 1000037

; Exercise 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;(timed-prime-test 1009) - 12
;(timed-prime-test 1013) - 7
;(timed-prime-test 1019) - 8
;(timed-prime-test 10007) - 13
;(timed-prime-test 10009) - 13
;(timed-prime-test 10037) - 13
;(timed-prime-test 100003) - 31
;(timed-prime-test 100019) - 32
;(timed-prime-test 100043) - 33
;(timed-prime-test 1000003) - 88
;(timed-prime-test 1000033) - 87
;(timed-prime-test 1000037) - 89
; around 78% ratio - not twice the speedup since we added a predicate, if, and an extra function call

; Exercise 1.24
;(timed-prime-test 1009) - 36
;(timed-prime-test 1013) - 29
;(timed-prime-test 1019) - 28
;(timed-prime-test 10007) - 33
;(timed-prime-test 10009) - 36
;(timed-prime-test 10037) - 32
;(timed-prime-test 100003) - 37
;(timed-prime-test 100019) - 37
;(timed-prime-test 100043) - 37
;(timed-prime-test 1000003) - 41
;(timed-prime-test 1000033) - 41
;(timed-prime-test 1000037) -54

; Exercise 1.25
; Yes, I think this procedure serves well. Same results but takes longer since Scheme has to use huge intermediate results rather than an iterative process.

; Exercise 1.26
; Applicative-order evaluation, when using 'square' it will evaluate the parameter first which is one recursive call but using '*' will double the amount of recursive calls log(2)^n = n*log(2) = n

; Exercise 1.27

(define (carmich-test a n) ; Incorrect
  ((cond ((= a n) 3333)
     ((= (expmod a n n) a) (carmich-test (+ a 1) n))
       (else (display "wrong")))))

; Exercise 1.28 ; Incorrect

; Exercise 1.29
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (h-inc c) (+ c h h))
  (* (/ h 3) (+ (f a) (* 2 (sum f a h-inc b)) (* 4 (sum f (+ a h) h-inc b)) (f b))))
;(simpson-rule cube 0 1.0 100)

; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (product identity 1 inc n))
(define (product-pi-approx n) ; Incorrect
  (define (inc x) (+ x 1))
  (define (double x) (* 2 x))
  (define (double-inc x) (+ (* 2 x) 1))
  (* 4.0 (/ (* 2 (square (product double 2 inc n))) (* 3 (square (product double-inc 2 inc n))))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (accumulate-sum n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (accumulate + 0 identity 1 inc n))
(define (accumulate-prod n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (accumulate * 1 identity 1 inc n))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (filtered-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (filtered-prod-prime a b)
  (define (gcd-filter x) (= (gcd x b) 1))
  (filtered-accumulate * 1 identity a inc b gcd-filter))

; Exercise 1.34
;(f f)
;(f 2)
;(2 2) - error since 2 is not a procedure

; Exercise 1.35
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio-fp x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) x))

;(golden-ratio-fp 1.0)

; Exercise 1.36
; (fixed-point (lambda (x)(/ (log 1000) (log x))) 2.0) - 34 approximations
; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0) - 9 approximations

; Exercise 1.37
(define (cont-frac n d k)
  (define (try i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (try (+ i 1))))))
  (try 1))

; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
(define (cont-frac-iter n d k)
  (define (try i result)
    (if (= 0 i)
        result
        (try (- i 1) (/ (n i) (+ (d i) result)))))
  (try (- k 1) (/ (n k) (d k))))

; Exercise 1.38
;(cont-frac (lambda (i) 1.0) (lambda (i)
;                              (if (= (remainder i 3) 2)
;                                  (* 2 (/ (+ i 1) 3))
;                                1)) 10)

; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x -1))) (lambda (i) (- (* 2.0 i) 1)) k))

; Exercise 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- g (+ x dx)) (g x)) dx))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; Exercise 1.41
(define (double-proc p) ; procedure has to have one argument
  (lambda (x) (p (p x))))

; (((double-proc (double-proc double-proc)) inc) 5)

; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43
(define (repeated p n)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (compose p result))))
  (iter n p))
; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (smooth-n f n)
  ((repeated smooth n) f))

; Exercise 1.45
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))
(define (nth-root n x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (power y (- n 1))))) 1.0))

; Exercise 1.46 ; Didn't do fixed point
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt-iter-imp x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))
