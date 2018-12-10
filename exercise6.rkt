#lang racket

;check matrix whether every row contains a number divisible by k
(define (checkMatrix? m k)
  (all? (lambda (l) (not (null? (filter (lambda (x) (= 0 (remainder x k))) l)))) m))


;min sum digit
(define (digits num)
  (if (< num 10)
      (list num)
      (cons (remainder num 10) (digits (quotient num 10)))))

(define (sum-digit num)
  (foldr + 0 (digits num)))

(define (min-sum-digit a b k)
  (if (> a b)
      b
      (if (zero? (remainder (sum-digit a) k))
          a
          (min-sum-digit (+ a 1) b k))))

;occurrences
(define (occurrences lst1 lst2)
  (foldr (lambda (el res) (cons (count el lst2) res)) '() lst1))

;match length
(define (match-lengths? ll1 ll2)
  (define lll1 (map length ll1))
  (define lll2 (map length ll2))
  (define diffs (map - lll1 lll2))
  (null? (filter (lambda (x) (not (equal? x (car diffs)))) diffs)))

;image
(define (image l1 l2)
  (define diffs (map - l1 l2))
  (null? (filter (lambda (x) (not (equal? x (car diffs)))) diffs)))


;average, calcprod
(define (average f g)
  (lambda (x) (quotient (+ (f x) (g x)) 2)))

(define (1+ x) (+ 1 x))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next)))) 

(define (const x) (lambda (y) x)) 

(define (calcprod f n)
  (lambda (x) (accumulate * 1 1 n (lambda (i) ((average f (const (expt i x))) i)) 1+))) 
  

;sublist? lst1 of lst2
(define (begins-with lst1 lst2)
  (or (null? lst1)
      (and (not (null? lst2))
           (equal? (car lst1) (car lst2))
           (begins-with (cdr lst1) (cdr lst2)))))

(define (sublist? lst1 lst2)
  (cond
    [(null? lst2) (null? lst1)]
    [(begins-with lst1 lst2) #t]
    [else (sublist? lst1 (cdr lst2))]))


;make set of lst
(define (insert-once x lst)
  (if (member x lst)
      lst
      (cons x lst)))
  
(define (make-set lst)
  (foldr insert-once '() lst))


;histogram
(define (count x lst)
    (length (filter (lambda (el) (equal? el x)) lst)))

(define (histogram lst)
  (define (insert x lst1)
    (cons (cons x (count x lst)) lst1)) 
  (foldr insert '() (make-set lst)))


;transpose
(define (transpose m)
  (apply map list m))


;triangular? matrix
(define (above-diag m)
  (if (null? m)
      '()
      (cons (cdr (get-first-row m)) (above-diag (map cdr (cdr m))))))

(define (all? p lst)
  (equal? lst (filter p lst)))

(define (triangular? m)
  (define tm (transpose m))
  (null? (filter (lambda (row) (not (all? zero? row))) (above-diag tm))))
 
  
;main diagonal
(define (get-first-row m) (car m))

(define (main-diag m)
  (if (null? m)
      '()
      (cons (car (get-first-row m)) (main-diag (map cdr (cdr m))))))


;secondary diagonal
(define (2nd-diag m)
  (reverse (main-diag (transpose m))))


;decartes
(define (combine x lst)
  (if (null? lst)
      '()
      (cons (cons x (car lst)) (combine x (cdr lst)))))
                  
(define (decartes lst1 lst2)
  (foldr (lambda (x res) (append (combine x lst2) res)) '() lst1))
  