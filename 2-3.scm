#lang scheme

; 2.54
(define (equal? l1 l2)
  (if (or (null? l1) (null? l2))
      (and (null? l1) (null? l2))
      (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))

; 2.57

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type: DERIV" exp))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum . ts)
  (define (combine-terms ts)
    (if (null? ts)
        '()
        (let ((rest (combine-terms (cdr ts)))
              (first (car ts)))
             (if (null? rest)
                 (list first)
                 (let ((r (car rest))   ; if rest has a number, it is r
                       (rs (cdr rest)))
                      (cond ((=number? first 0) rest)
                            ((=number? r 0) (cons first rs))
                            ((and (number? first) (number? r))
                             (cons (+ first r) rs))
                            ((number? r) (cons r (cons first rs)))
                            (else (cons first rest))))))))
  (let ((res (combine-terms ts)))
       (if (= 1 (length res))
           (car res)
           (cons '+ res))))

(define (make-product . fs)
  (define (combine-factors fs)
    (if (null? fs)
        '()
        (let ((rest (combine-factors (cdr fs)))
              (first (car fs)))
             (if (null? rest)
                 (list first)
                 (let ((r (car rest))
                       (rs (cdr rest)))
                     (cond ((or (=number? first 0) (=number? r 0)) (list 0))
                           ((=number? first 1) rest)
                           ((=number? r 1) (cons first rs))
                           ((and (number? first) (number? r))
                            (cons (* first r) rs))
                           ((number? r) (cons r (cons first rs)))
                           (else (cons first rest))))))))
  (let ((res (combine-factors fs)))
       (if (= 1 (length res))
           (car res)
           (cons '* res))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define addend cadr)
(define (augend x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '+ (cddr x))))

(define multiplier cadr)
(define (multiplicand x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '* (cddr x))))

; 2.61
(define (adjoin-set e s)
  (cond ((null? s) (list e))
        ((= e (car s)) s)
        ((< e (car s)) (cons e s))
        ((> e (car s)) (cons (car s) (adjoin-set e (cdr s))))))
; 2.62

(define (union-set s1 s2)
  (cond ((or (null? s1) (null? s2)) (append s1 s2))
        ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
        ((> (car s1) (car s2)) (cons (car s2) (union-set s1 (cdr s2))))))
