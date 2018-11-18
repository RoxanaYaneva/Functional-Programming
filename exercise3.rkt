#lang racket
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (filter-acum p? op nv a b term next)
  (cond
    [(> a b) nv]
    [(p? a) (op (term a) (filter-acum p? op nv (next a) b term next))]
    [(filter-acum p? op nv (next a) b term next)]))


(define (!! n)
  (accumulate * 1 (if (odd? n) 1 2) n (lambda (x) x) (lambda (x) (+ x 2))))

(define (id x) x)
(define (1+ k) (+ k 1))

(define (nchk n k)
  (define (fact n) (accumulate * 1 1 n id 1+))
  (/ (fact n) (fact k) (fact (- n k))))

(define (2^ n)
  (accumulate * 1 1 n (lambda (x) 2) 1+))


(define (count p? a b)
  (filter-acum p? + 0 a b (lambda (x) 1) 1+))


(define (divisors-sum n)
  (filter-acum
   (lambda (u) (= 0 (remainder n u))) + 0 1 n id 1+))


(define (num-digits n)
  (if (< n 10)
        1
        (+ 1 (num-digits (quotient n 10)))))

(define (middle-digit n)
  (define mid-index (quotient (num-digits n) 2))
  (define (iter i num)
    (if (= i mid-index)
        (remainder num 10)
        (iter (+ i 1) (quotient num 10))))
  (if (even? (num-digits n))
      -1
      (iter 0 n)))


(define (meetTwice? f g a b)
  (define (iter a b cnt)
    (cond
      [(> a b) #f]
      [(= cnt 2) #t]
      [(= (f a) (g a)) (iter (+ a 1) b (+ cnt 1))]
      [else (iter (+ a 1) b cnt)]))
  (iter a b 0))
        