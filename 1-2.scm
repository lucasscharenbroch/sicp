#lang scheme

; 1.11
; recursive process
(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1))
         (* (fr (- n 2))
            2)
         (* (fr (- n 3))
            3))))

; iterative process
(define (fi n)
  (define (f-iter a b c count-to-go)
    (if (= count-to-go 0)
        c
        (f-iter b c (+ (* 3 a) (* 2 b) c) (- count-to-go 1))))
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 2))))

; 1.12
(define (choose n k)
  (cond ((= n 0) 1)
        ((= k 0) 1)
        ((= n k) 1)
        ((+ (choose (- n 1) (- k 1)) (choose (- n 1) k)))))

; 1.16
(define (exp b e)
  (define (exp-iter b e a)
    (cond ((= e 0) a)
          ((even? e) (exp-iter (* b b) (/ e 2) a))
          (else (exp-iter b (- e 1) (* a b)))))
  (exp-iter b e 1))

; 1.17
(define (fast-mul x y)
  (define (double z) (+ z z))
  (define (half z) (/ z 2))
  (cond ((< x 0) (- (fast-mul (- x) y)))
        ((= x 0) 0)
        ((even? x) (double (fast-mul (half x) y)))
        (else (+ y (fast-mul (- x 1) y)))))

; 1.18
(define (fast-mul-it x y)
  (define (double z) (+ z z))
  (define (half z) (/ z 2))
  (define (iter x y a)
    (cond ((= x 0) a)
          ((even? x) (iter (half x) (double y) a))
          (else (iter (- x 1) y (+ a y)))))
  (if (< x 0)
      (- (iter (- x) y 0))
      (iter x y 0)))
