 #lang sicp
; Auxiliary Functions
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-interval a b) (cons a b))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (square x)
  (* x x))
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (define (iter result rest)
    (cond ((null? rest) result)
          ((= (length rest) 1) (iter (op result (op (car rest) initial)) (cdr rest)))
          (else (iter (op (car rest) (cadr rest)) (cddr rest)))))
  (iter initial sequence))
; unordered
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
(define (key given-key) given-key)
(define (entry tree) (cdar tree))
; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (num (if (< d 0)
                 (* n -1)
                 n))
        (den (if (< d 0)
                 (* d -1)
                 d)))
    (if (< g 0)
        (cons (/ num (* g -1)) (/ den (* g -1)))
        (cons (/ num g) (/ den g)))))

; Exercise 2.2
; Don't have to worry about point implementation until after segment
(define (make-segment start_pt end_pt) (cons start_pt end_pt))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

(define (midpoint-segment seg)
  (let (
        (start_x (x-point (start-segment seg)))
        (start_y (y-point (start-segment seg)))
        (end_x (x-point (end-segment seg)))
        (end_y (y-point (end-segment seg)))
        )
    (make-point (/ (+ start_x end_x) 2) (/ (+ start_y end_y) 2))))

; Exercise 2.3
; Using two diagonally opposite points 
(define (make-rect pt1 pt2) (cons pt1 pt2))
(define (p1 rect) (car rect))
(define (p2 rect) (cdr rect))
; Using diagonal segment - idk if this is cheating?
(define (make-rect2 seg) (cons (start-segment seg) (end-segment seg)))


(define (perim rect)
  (let
      (
       (x-length (abs (- (x-point (p1 rect)) (x-point (p2 rect)))))
       (y-length (abs (- (y-point (p1 rect)) (y-point (p2 rect)))))
      )

      (+ (* 2 x-length) (* 2 y-length))))

(define (area rect)
    (let
      (
       (x-length (abs (- (x-point (p1 rect)) (x-point (p2 rect)))))
       (y-length (abs (- (y-point (p1 rect)) (y-point (p2 rect)))))
      )

      (* x-length y-length)))

; Exercise 2.4 -- commented to not interfere with other exercises
; Given: cons and car
; (define (cons x y)
;   (lambda (m) (m x y)))
; (define (car z)
;   (z (lambda (p q) p)))

; (define (cdr z)
;   (z (lambda (p q) q)))
; Verification (Idc about brackets):
; cdr (cons x y)
;   => (cons x y) (lambda (p q) q)
;   => (lambda (m) (m x y)) (lambda (p q) q)
;   => ((lambda (p q) q) x y) ; Using beta reduction
;   => (lambda (x y) y)
;   => y

; Exercise 2.5
(define (cons1 a b)
  (* (expt 2 a) (expt 3 b)))

; keep dividing by two
(define (car1 c)
  (define (car-iter total count)
    (if (= (remainder total 2) 0)
        (car-iter (/ total 2) (+ count 1))
        count))
  (car-iter c 0))
; keep dividing by three
(define (cdr1 c)
  (define (cdr-iter total count)
    (if (= (remainder total 3) 0)
        (cdr-iter (/ total 3) (+ count 1))
        count))
  (cdr-iter c 0))

; Exercise 2.6
; Given: zero and add-1

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Substitution:
; add-1 zero
;   => lambda (f) (lambda (x) (f ((zero f) x)))
;   => lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))
;   => lambda (f) (lambda (x) (f (lambda (x) x)) x)
;   => lambda (f) (lambda (x) (f x))

; two: lambda (f) (lambda (x) (f (f x)))


(define (church-add m n)
  (lambda (f) (lambda (x) (m (f ((n f) x))))))
; Substitution
; church-add one two
;  => lambda (f) (lambda (x) (one (f ((two f) x))))
;  => lambda (f) (lambda (x) ((lambda (f) (lambda (x) (f x))) (f (((lambda (f) (lambda (x) (f (f x))))) f) x)))
;  => lambda (f) (lambda (x) ((lambda (f) (lambda (x) (f x))) (f ((lambda (x) (f (f x))) x))))
;  => lambda (f) (lambda (x) ((lambda (f) (lambda (x) (f x))) (f (f (f x)))))
;  => lambda (f) (lambda (x) ((lambda (x) ((f (f (f x))) x)))) ; I screwed up the brackets here
;  => lambda (f) (lambda (x) (f (f (f x)))

; Exercise 2.7
(define (upper-bound interval) (max (cdr interval) (car interval)))
(define (lower-bound interval) (min (cdr interval) (car interval)))

; Exercise 2.8
; idk if this logic fits
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; Exercise 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
(define (add-width x y)
  (+ (width-interval x) (width-interval y)))

; Exercise 2.10
(define (div-chk-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "interval spans 0")
      (div-interval x y)))

; Exercise 2.11
; works with my min max dfn in 2.7? (I'm confused)

; Exercise 2.12
(define (make-center-percent c p) ; assuming p is decimal
  (let (
        (diff (* c p))
        )
    (make-interval (- c diff) (+ c diff))))
(define (percent i)
  (/ (width i) (center i)))

; Exercise 2.13
; Assuming small percentage tolerances
; (mul-interval (make-center-percent c1 p1) (make-center-percent c2 p2))
; => (make-interval (* (- c1 (* c1 p1)) (- c2 (* c2 p2))) (* (+ c1 (* c1 p1)) (+ c2 (* c2 p2))))
; let l1 = (- c1 (* c1 p1)), l2 = (- c2 (* c2 p2)), u1 = (+ c1 (* c1 p1)), u2 = (+ c2 (* c2 p2))


; => (percent (make-interval (* l1 l2) (* u1 u2)))
; => (/ (width (make-interval (* l1 l2) (* u1 u2))) (center (make-interval (* l1 l2) (* u1 u2))))
; Width Simplification:
; => (/ (- (* u1 u2) (* l1 l2)) 2)
; Center Simplification:
; => (/ (+ (* u1 u2) (* l1 l2)) 2)
; Width / Center:
; => (/ (/ (- (* u1 u2) (* l1 l2)) 2) (/ (+ (* u1 u2) (* l1 l2)) 2))
; => (* (/ (- (* u1 u2) (* l1 l2)) 2) (/ 2 (+ (* u1 u2) (* l1 l2))))
; => (/ (- (* u1 u2) (* l1 l2)) (+ (* u1 u2) (* l1 l2)))
; => (/ (- (* (+ c1 (* c1 p2)) (+ c2 (* c2 p2))) (* (- c1 (* c1 p1)) l2 = (- c2 (* c2 p2)))) (+ (* (+ c1 (* c1 p2)) (+ c2 (* c2 p2))) (* (- c1 (* c1 p1)) l2 = (- c2 (* c2 p2)))))
; => (/ (+ (* c1 c2) (* c1 c2 p2) (* c1 c2 p2) (* c1 c2 p1 p2) (- (* c1 c2)) (* c1 c2 p2) (* c1 c2 p1) (- (* c1 c2 p1 p2)))
;       (+ (* c1 c2) (* c1 c2 p2) (* c1 c2 p2) (* c1 c2 p1 p2) (* c1 c2) (- (* c1 c2 p2)) (- (* c1 c2 p1)) (* c1 c2 p1 p2)))
; => (/ (+ (* 2 c1 c2 p1) (* 2 c1 c2 p2)) (* 2 c1 c2))
; => (+ p1 p2) therefore, for small percentage tolerances, the tolerance for multiplication of intervals is the same as adding their percentage tolerances

; Exercise 2.14 -- TODO

; Exercise 2.15 -- TODO

; Exercise 2.16 -- TODO

; Exercise 2.17
(define (last-pair xs)
  (if (= (length xs) 1)
      xs ; returns the list of the last element
      (last-pair (cdr xs))))

; Exercise 2.18
(define (reverse xs)
  (define (reverse-iter res xs count)
    (if (= count 0)
        res
        (reverse-iter (cons (car xs) res) (cdr xs) (- count 1))
    )
  )
  (reverse-iter nil xs (length xs)))

; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values) ; (first-denomination us-coins) => (50)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

; Exercise 2.20
(define (same-parity . y)
  (define (same-parity-iter parity xs result)
    (cond ((null? xs) (reverse result))
          ((parity (car xs)) (same-parity-iter parity (cdr xs) (cons (car xs) result)))
        (else (same-parity-iter parity (cdr xs) result))))
  (if (= (remainder (car y) 2) 0)
      (same-parity-iter even? y nil)
      (same-parity-iter odd? y nil)))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list2 items)
  (map square items))

; Exercise 2.22 -- TODO: Revisit this
; List is reversed since he is doing cons of the earlier elements before the adding in the later elements which will be stacked on top. (FILO)

; The empty list will be in the front and the rest of the list will be nested

; Exercise 2.23
(define (for-each f xs)
  (if (null? xs)
      #t
      ((lambda (x) (f (car x)) (for-each f (cdr x))) xs)))

; Exercise 2.24
; (1 (2 (3 4)))

; -- -> -x
; |     |     
; v     v     
; 1     -- -> -x
;       |    |
;       v    v
;       2    -- -> -x
;            |     |
;            v     v
;            3     4

; Exercise 2.25
; (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9) )))))
; (car (car (list (list 7))))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))) ))))))))))))

; Exercise 2.26
; (define x (list 1 2 3))
; (define y (list 4 5 6))
; (append x y) ; prediction: (1 2 3 4 5 6)
; (cons x y)   ; prediction: (1 2 3 (4 5 6)), wrong it's ((1 2 3) 4 5 6) since car wouldn't work
; (list x y)   ; prediction: (1 2 3 (4 5 6)), wrong it's ((1 2 3) (4 5 6)) since car and cdr wouldn't work

; Exercise 2.27
(define (deep-reverse xs)
  (define (deep-reverse-iter res xs count)
    (if (= count 0) ; pair?
        res
        (deep-reverse-iter (cons (deep-reverse (car xs)) res) (cdr xs) (- count 1))
      )
    )
  (if (pair? xs)
  (deep-reverse-iter nil xs (length xs))
  xs
  ))

; Exercise 2.28 - pg 111
(define (fringe xs)
  (cond ((null? xs) nil)
        ((pair? (car xs)) (append (fringe (car xs)) (fringe (cdr xs))))
        (else xs)
))


; Exercise 2.29
;a 
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
;b
(define (total-weight mobile)
  (cond ((and (integer? (right-branch mobile)) (integer? (left-branch mobile))) (right-branch mobile))
        ((integer? (left-branch mobile)) (total-weight (right-branch mobile)))
        (else (+ (total-weight (left-branch mobile)) (total-weight (right-branch mobile))))))
;c
(define (balanced? mobile)
  (define (torque branch)
    (cond ((and (integer? (left-branch branch)) (integer? (right-branch branch))) (* (left-branch branch) (right-branch branch)))
          ((and (integer? (left-branch branch)) (not (integer? (right-branch branch)))) (torque (right-branch branch)))
          ((not (and (integer? (left-branch branch)) (integer? (right-branch branch)))) (+ (torque (left-branch branch)) (torque (right-branch branch))))
    ))
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))
;d
;I only need to change (right-branch mobile) from using 'cadr' to 'cdr'

 
; Exercise 2.30
(define (square-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (square t))
        (else (cons (square-tree (car t)) (square-tree (cdr t))))))
(define (square-tree-map t)
  (map (lambda (sub-t)
         (if (pair? sub-t)
             (square-tree-map sub-t)
             (square sub-t)))
       t))
         
; Exercise 2.31
;(define (tree-map proc tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (proc tree))
;        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define (tree-map proc tree)
  (map (lambda (sub-t)
         (if (pair? sub-t)
             (tree-map proc sub-t)
             (proc sub-t)))
       tree))


; Exercise 2.32 - needed help
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))rest)))))
; Append powerset of set without first element and powerset of set without first element and adding the first element to all of it.

; Exercise 2.33 - pg 119
(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append-acc seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-acc sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
               0
               coefficient-sequence))

; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map
                   (lambda (sub-t) (cond ((not(pair? sub-t)) 1)
                                         (else (count-leaves sub-t))))
                   t)))
; Exercise 2.36 -- crazy how something so high-level could define all these functions
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; Exercise 2.37
;(define (dot-product v w)
;  (accumulate + 0 (map * v w))) ; extended version of map
;(define (matrix-*-vector m v)
;  (map (lambda (mi) (dot-product mi v)) m))
;(define (transpose mat)
;  (accumulate-n cons nil mat))
;(define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))
;    (map (lambda (mi) (matrix-*-vector cols mi)) m)))
; Exercise 2.38
;(fold-right / 1 (list 1 2 3)) ; 1/6
;(fold-left  / 1 (list 1 2 3)) ; 1/6
;(fold-right list nil (list 1 2 3)) ; ((1 2) (3 ()))
;(fold-left list nil (list 1 2 3))  ; (((() 1) 2) 3)
; The property 'op' needs to satisfy is commutative.
; Exercise 2.39
(define (reverse-foldr sequence)
  (fold-right (lambda (x y) (cons y x)) nil sequence)) ; returns ((() . 3) 2 . 1)
(define (reverse-foldl sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

; Exercise 2.40 -- TODO
; Exercise 2.41 -- TODO
; Exercise 2.42 -- TODO: gave up on safe?
; filter pred seq
; flatmap proc seq
; accumulate op init seq
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens) ; [1..8] 4 [(1 4) (2 3) ..] --> I completely overcomplicated this
  (cons (list new-row k) rest-of-queens))
(define empty-board nil)
(define (safe? k positions) ; Solution online - dont understand how diagonals work
  (define (member-test val seq)
    (cond ((null? seq) #t)
          ((= val (car seq)) #f) ; false, since it matches row/col with another queen
          (else (member-test val (cdr seq)))))
  (define (comparison-test seq1 seq2)
    (cond ((null? seq1) #t)
          ((= (car seq1) (car seq2)) #f)
          (else (comparison-test (cdr seq1) (cdr seq2)))))
 (let ((rows (map car positions))
          (cols (map cadr positions)))
         (and (member-test (car rows) (cdr rows))
              (member-test (car cols) (cdr cols))
              (comparison-test (map (lambda (i) (abs (- i (car rows)))) ; Comparing absolute value of row-other_row
                                    (cdr rows))
                               (map (lambda (i) (abs (- i (car cols)))) ; Comparing absolute value of col-other_col for diagonal
                                    (cdr cols))))))

; Exercise 2.43 -- TODO
; Exercise 2.44 -- TODO
; Exercise 2.45 -- TODO
; Exercise 2.46 -- TODO
; Exercise 2.47 -- TODO
; Exercise 2.48 -- TODO
; Exercise 2.49 -- TODO
; Exercise 2.50 -- TODO
; Exercise 2.51 -- TODO
; Exercise 2.52 -- TODO

; Exercise 2.53
; (list 'a 'b 'c) ; (a b c)
; (list (list 'george)) ; ((george))
; (cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
; (cadr '((x1 x2) (y1 y2))) ; (y1 y2)
; (pair? (car '(a short list))) ; #f
; (memq 'red '((red shoes) (blue socks))) ; #f
; (memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

; Exercise 2.54
;(define (equal? a b)
;  (cond ((not (= (length a) (length b))) #f)
;  ((and (null? a) (null? b)) #t)
;  (else (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
;      ))

; Exercise 2.55
; (car ''abracadabra) ; Since this is (car (quote (quote abracadabra)))
; Exercise 2.56
; Exercise 2.57
; Exercise 2.58

; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((element-of-set? (car set2) set1)
           (union-set set1 (cdr set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))
; Exercise 2.60
; I wouldn't change element-of-set?
; I would change adjoin-set to not have a check and just 'cons'
; Union-set would be the same
; I would leave intersection-set the same, since they would just return a list with duplicates
; The applications would be adjoin-heavy operations
; Exercise 2.61
(define (adjoin-set-ordered x set) ; didn't do the base cases or use '='
  (if (< x (car set))
      (cons x set)
      (cons (car set) (adjoin-set-ordered x (cdr set)))))
; Exercise 2.62
(define (union-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
  (let (
        (x1 (car set1))
        (x2 (car set2)))
    (cond ((= x1 x2)
           (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
          ((< x1 x2)
           (union-set-ordered (cdr set1) set2))
          ((< x2 x1)
           (union-set-ordered set1 (cdr set2)))))))
; Exercise 2.63
;a
; I think both procedures produce the same result for every tree as it goes left branch then entry and right branch (i.e. In-Order Traversal)
;b
; It depends on if the tree is balanced.
; list->tree1 is O(n * log n)
; list->tree2 is O(n)
; Exercise 2.64 -- TODO
; Exercise 2.65 -- TODO
; Exercise 2.66
(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((eq? given-key (key (entry tree))) (entry tree))
        ((< given-key (key (entry tree))) (lookup given-key (left-branch tree)))
        ((> given-key (key (entry tree))) (lookup given-key (right-branch tree)))))
; Exercise 2.67 -- TODO
; Exercise 2.68 -- TODO
; Exercise 2.69 -- TODO
; Exercise 2.70 -- TODO
; Exercise 2.71 -- TODO
; Exercise 2.72 -- TODO

; Exercise 2.73
; Exercise 2.74
; Exercise 2.75
; Exercise 2.76

; Exercise 2.77
; Exercise 2.78
; Exercise 2.79
; Exercise 2.80

; Exercise 2.81
; Exercise 2.82
; Exercise 2.83
; Exercise 2.84
; Exercise 2.85
; Exercise 2.86

; Exercise 2.87
; Exercise 2.88
; Exercise 2.89
; Exercise 2.90
; Exercise 2.91

; Exercise 2.92
; Exercise 2.93
; Exercise 2.94
; Exercise 2.95
; Exercise 2.96
; Exercise 2.97

