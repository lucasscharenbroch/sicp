#lang scheme

; 2.2
(define x-pt car)
(define y-pt cdr)
(define mk-pt cons)

(define start-seg car)
(define end-seg cdr)
(define mk-seg cons)

(define (midpoint s)
  (let* ((p1 (start-seg s))
         (p2 (end-seg s))
         (x1 (x-pt p1))
         (x2 (x-pt p2))
         (y1 (y-pt p1))
         (y2 (y-pt p2)))
        (mk-pt (/ (+ x1 x2) 2.0)
               (/ (+ y1 y2) 2.0))))

; 2.5
(define (xcons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (xcar x)
  (cond ((= 0 x) 0)
        ((not (= 0 (remainder x 2))) 0)
        (else (+ 1 (xcar (/ x 2.0))))))

(define (xcdr x)
  (cond ((= 0 x) 0)
        ((not (= 0 (remainder x 3)) ) 0)
        (else (+ 1 (xcdr (/ x 3.0))))))

; 2.6
(define (compose f g) (lambda (x) (f (g x))))

(define (zero f) (lambda (x) x))
(define (one f) (lambda (x) (f x)))
(define (two f) (lambda (x) (f (f x))))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
(define (add n m) (lambda (f) ((compose n m) f)))
