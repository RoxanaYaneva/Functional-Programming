#lang racket

;Task 1 - a)

(define (digits-list n)
  (if (< n 10)
      (list n)
      (cons (remainder n 10) (digits-list (quotient n 10)))))

(define (narcissistic? n)
  (define num-digits (length (digits-list n)))
  (= n (foldr + 0 (map (lambda (x) (expt x num-digits)) (digits-list n)))))


; Task 1 - b)

(define (divides? n i) (zero? (remainder n i)))

(define (divisors-list n i)
  (cond
    [(= n i) '()]
    [(divides? n i) (cons i (divisors-list n (+ i 1)))]
    [else (divisors-list n (+ i 1))]))

(define (divisors-sum n)
   (foldr + 0 (divisors-list n 1)))

(define (friendly? a b)
  (define da (divisors-sum a))
  (define db (divisors-sum b))
  (and (= da b) (= db a)))


; Task 3 - a)

(define il '((24 . 26) (90 . 100) (0 . 100) (10 . 89) (1 . 5) (-4 . 25)))

(define (dist pair)
  (- (cdr pair) (car pair)))

(define (shortest-interval-supersets il)
  (define shortest-interval
    (foldr (lambda (x y) (if (< (dist x) (dist y)) x y)) (car il) il))
  (define (subinterval? i1 i2)
    (and
     (>= (car i1) (car i2))
     (<= (cdr i1) (cdr i2))))
  (filter (lambda (i) (subinterval? shortest-interval i)) il))


; Task 3 - b)
(define (cdr-pair p1 p2)
  (> (cdr p1) (cdr p2)))

(define (insert compare pair lst)
  (cond
    [(null? lst) (cons pair '())]
    [(compare pair (car lst)) (cons (car lst) (insert compare pair (cdr lst)))]
    [else (cons pair lst)]))

(define (insertion-sort compare lst)
  (define (insert-compare pair list) (insert compare pair list))
    (foldr insert-compare '() lst))

(define (shortest-interval-supersets1 lst)
  (insertion-sort cdr-pair (shortest-interval-supersets lst)))
