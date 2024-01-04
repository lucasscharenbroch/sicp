#lang scheme

; 3.1
(define (make-accumulator val)
  (lambda (x)
    (begin (set! val
                 (+ val x))
           val)))

; 3.2
(define (make-monitored f)
  (let ((num-calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) num-calls)
            ((eq? arg 'reset-count) (set! num-calls 0))
            (else (set! num-calls (+ num-calls 1))
                  (f arg))))))

; 3.3, 3.4, 3.7
(define (call-the-cops) (println "Local police-officer dispached"))

(define (elem e l)
  (if (null? l)
      #f
      (or (eq? e (car l))
          (elem e (cdr l)))))

(define (make-account balance password)
  (define (deposit amt)
    (set! balance (+ balance amt))
    balance)
  (define (withdraw amt)
    (if (< balance amt)
      "Insufficient Funds"
      (begin
        (set! balance (- balance amt))
        balance)))
  (let ((n-bad-attempts 0)
        (passwords (list password)))
  (define (jointify pw)
    (set! passwords (cons pw passwords)))
    (define (dispatch pw op)
      (if (not (elem pw passwords))
          (begin
            (set! n-bad-attempts (+ 1 n-bad-attempts))
            (if (> n-bad-attempts 7)
                (lambda (_) (call-the-cops) "Invalid Password...")
                (lambda (_) "Invalid Password")))
          (begin
            (set! n-bad-attempts 0)
            (cond ((eq? op 'deposit) deposit)
                  ((eq? op 'withdraw) withdraw)
                  ((eq? op 'jointify) jointify)
                  (else (error "Unknown Request: MAKE-ACCOUNT" op))))))
    dispatch))

; 3.7
(define (make-joint acc pw1 pw2)
  ((acc pw1 'jointify) pw2)
  acc)

; 3.8
(define counter 0)
(define (f x)
  (let ((res (if (= x counter)
                 0
                 x)))
    (set! counter (+ counter 1))
    res))
