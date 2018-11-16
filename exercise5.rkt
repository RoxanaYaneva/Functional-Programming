#lang racket

;lenght of list
(define (lenght list)
  (if (null? list)
      0
      (+ 1 (lenght (cdr list)))))

;reverse list
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))

;map list
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))

;filter list
(define (filter p? list)
  (cond
    [(null? list) list]
    [(p? (car list)) (cons (car list) (filter p? (cdr list)))]
    [else (filter p? (cdr list))]))

;nth element of a list
(define (nth n lst)
  (cond
    [(null? lst) #f]
    [(zero? n) (car lst)]
    [else (nth (- n 1) (cdr lst))]))

;range from to
(define (range from to)
  (if (> from to) '()
      (cons (range (+ from 1) to))))

;digit list
(define (digits-list n)
  (if (< n 10) (list n)
      (append (digits-list (quotient n 10)) (list (remainder n 10)))))

;take n
(define (take n lst)
  (cond
    [(or (null? lst) (zero? n)) '()]
    [(= n 1) (list (car lst))]
    [else (cons (car lst) (take (- n 1) (cdr lst)))]))

;drop n
(define (drop n lst)
  (cond
    [(null? lst) '()]
    [(= n 0) lst]
    [else (drop (- n 1) (cdr lst))]))

;all? p? l
(define (all? p? l)
  (define ll (length l))
  (define lf (length (filter p? l)))
  (= ll lf))

;any p? l
(define (any? p? l)
  (define lf (length (filter p? l)))
  (>= lf 1))
    
;zip list1 list2
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))

;zipWith f list1 list2
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (car lst1) (car lst2)) (zipWith f (cdr lst1) (cdr lst2)))))

;sorted? list
(define (sorted? lst)
  (cond
    [(null? (cdr lst)) #t]
    [(> (car lst) (car (cdr lst))) #f]
    [else (sorted? (cdr lst))]))
       
;uniques list
(define (remove x xs)
  (filter (lambda (y) (not (equal? x y))) xs))

(define (uniques lst)
  (if (null? lst)
      lst
      (cons (car lst) (uniques (remove (car lst) (cdr lst))))))

;extract integers
(define (extract-ints lst)
  (filter integer? lst))

;insert value
(define (insert val lst)
  (cond
    [(null? lst) (cons val '())]
    [(> val (car lst)) (cons (car lst) (insert val (cdr lst)))]
    [else (cons val lst)]))
          
;insertion sort
(define (insertion-sort lst)
  (foldr insert '() lst))

(define (next-look-and-say lst)
  (define (iter cnt lst)
    (cond
      [(null? lst) '()]
      [(null? (cdr lst)) (list cnt (car lst))]
      [(= (car lst) (cadr lst))  (iter (+ cnt 1) (cdr lst))]
      [else (cons cnt (cons (car lst) (next-look-and-say (cdr lst))))]))
  (iter 1 lst))