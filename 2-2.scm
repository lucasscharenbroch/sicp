#lang scheme

(define nil '())

(define (inc x) (+ 1 x))

; (define (map f l)
;   (if (null? l)
;       nil
;       (cons (f (car l)) (map f (cdr l)))))

; (define (filter f l)
;   (if (null? l)
;       nil
;       (let ((rest (filter f (cdr l))))
;            (if (f (car l))
;                (cons (car l) rest)
;                rest))))

(define (square x) (* x x))

; 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

; 2.18
(define (reverse l)
  (define (iter l a)
    (if (null? l) a
        (iter (cdr l) (cons (car l) a))))
  (iter l nil))

; 2.20
(define (same-parity x . xs)
  (filter (lambda (y) (= (remainder x 2) (remainder y 2))) (cons x xs)))

; 2.21
(define (square-list l)
  (map square l))

; 2.23
(define (for-each f l)
  (map f l)
  nil)

; 2.27
(define (deep-map f l)
  (define (fp x)
    (if (list? x)
        (f (map fp x))
        x))
  (f (map fp l)))

(define (deep-reverse l)
  (deep-map reverse l))

; 2.28
(define (fringe t)
  (cond ((null? t) nil)
        ((list? t) (append (fringe (car t)) (fringe (cdr t))))
        (else (list t))))

; 2.31
(define (tree-map f t)
  (cond ((null? t) nil)
        ((list? t) (cons (tree-map f (car t)) (tree-map f (cdr t))))
        (else (f t))))

(define (square-tree t) (tree-map square t))

; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
           (append rest
                   (map (lambda (x) (append (list (car s)) x))
                        rest)))))

; 2.33
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (map f seq)
  (define (f-then-cons x y)
    (cons (f x) y))
  (accumulate f-then-cons nil seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (inc y)) 0 seq))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
