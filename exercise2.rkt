#lang racket

(define (fast-exp x n)
  (cond
    [(= n 0) 1]
    [(= (remainder n 2) 0) (fast-exp (* x x) (quotient n 2))]
    [else (* x (fast-exp (* x x) (quotient n 2)))]))


(define (roots a b c)
  (define D (- (* b b) (* 4 a c)))
  (cond
    [(= D 0) 1]
    [(> D 0) 2]
    [(< D 0) 0]))


(define (fact n)
  (if (< n 1)
      1
      (* n (fact (- n 1)))))
     

(define (fib n)
  (define (iter i fi fi-1)
    (if (< i n)
        (iter (+ i 1) (+ fi fi-1) fi)
        fi))
  (if (= n 0) 0
      (iter 1 1 0)))


(define (reverse-int n)
  (define (iter n res)
    (if (< n 10) (+ (* res 10) n)
        (iter (quotient n 10) (+ (* res 10) (remainder n 10)))))
  (iter n 0))


(define (palindrome? n)
  (= n (reverse-int n)))


(define (prime? n)
  (define (iter i l)
    (cond
      [(= i n) #t]
      [(= 0 (remainder n i)) #f]
      [else (iter (+ i 1) l)]))
  (if (= n 1) #f
  (iter 2 (sqrt n))))


(define (increasing? n)
  (define last (remainder n 10))
  (define pre-last (remainder (quotient n 10) 10))
  (cond
       [(< n 10) #t]
       [(<= last pre-last) #f]
       [else (increasing? (quotient n 10))]))


(define (divisors-sum n)
  (define (iter i res)
    (cond
      [(> i n) res]
      [(= (remainder n i) 0) (iter (+ i 1) (+ res i))]
      [else (iter (+ i 1) res)]))
  (iter 1 0))


(define (perfect? n)
  (if (= n (- (divisors-sum n) n)) #t #f))


(define (bin->dec n)
  (if (zero? n)
      n
      (+ (remainder n 10) (* 2 (bin->dec (quotient n 10))))))

(define (dec->bin n)
  (if (zero? n)
      n
      (+ (remainder n 2) (* 10 (dec->bin (quotient n 2))))))