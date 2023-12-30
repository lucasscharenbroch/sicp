#lang scheme

(define (inc x) (+ x 1))

; 1.30
; recursive process (from book)
(define (sum-rec term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-rec term (next a) next b))))

; iterative process
(define (sum-iter term a next b)
  (define (iter i s)
    (if (> i b)
        s
        (iter (next i) (+ s (term i)))))
  (iter a 0))

; 1.31
; a
; recursive
(define (prod-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod-rec term (next a) next b))))

(define (factorial x) (prod-rec identity 1 inc x))

(define (approx-pi x)
  (define (term i)
    (let ((n (+ (inc i)
                (if (even? i) 1 0)))
          (d (+ (inc i)
                (if (odd? i) 1 0))))
         (* 1.0 (/ n d))))
  (* 4 (prod-rec term 1 inc x)))

; b
; iterative
(define (prod-iter term a next b)
  (define (iter i p)
    (if (> i b)
        p
        (iter (next i) (* p (term i)))))
  (iter a 1))

; 1.32
; a
; recursive
(define (acc-rec comb null term a next b)
  (if (> a b)
      null
      (comb (term a)
            (acc-rec comb null term (next a) next b))))

; b
; iterative
(define (acc-iter comb null term a next b)
  (define (iter i s)
    (if (> i b)
        s
        (iter (next i) (comb s (term i)))))
    (iter a null))

; 1.33
(define (fil-acc pred comb null term a next b)
  (cond ((> a b) null)
        ((not (pred (term a))) (fil-acc pred comb null term (next a) next b))
        (else (comb (term a)
                    (fil-acc pred comb null term (next a) next b)))))

; b
(define (prod-rel-prime n) (fil-acc (lambda (x) (= 1 (gcd x n))) * 1 identity 1 inc (- n 1)))

; 1.41
(define (double f) (lambda (x) (f (f x))))
(((double (double double)) inc) 5) ; 21

; 1.42
(define (compose f g) (lambda (x) (f (g x))))

; 1.43
(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))
