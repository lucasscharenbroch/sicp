#lang scheme

; 1.3
(define (sum-max-two-squares x y z)
  (define (square x) (* x x))
  (+ (square x) (square y) (square z) (- (square (min x y z)))))

; 1.8
(define (croot x)
  (define (improve y)
    (/ (+ (/ x (* y y))
          (* 2 y))
       3))
  (define (good-enough? y)
    (< (abs (- (* y y y)
               x))
       0.001))
  (define (try y)
    (if (good-enough? y)
        y
        (try (improve y))))
  (try 1.0))
