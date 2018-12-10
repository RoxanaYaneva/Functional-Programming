#lang racket

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))

(define empty-tree '())

(define (make-tree root left right) (list root left right))

(define (make-leaf root) (make-tree root empty-tree empty-tree))

(define root-tree car)

(define left-tree cadr)

(define right-tree caddr)

(define empty-tree? null?)


(define t
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

;tree sum
(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t) (tree-sum (left-tree t)) (tree-sum (right-tree t)))))

;tree max
(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (max (tree-max (left-tree t))
                (tree-max (right-tree t))))))

;tree level
(define (tree-level k t)
  (cond
    [(empty-tree? t) '()]
    [(zero? k) (list (root-tree t))]
    [(append (tree-level (- k 1) (left-tree t)) (tree-level (- k 1) (right-tree t)))]))
  
;all levels
(define (depth-tree t)
  (if (empty-tree? t)
      0
      (+ 1 (max (depth-tree (left-tree t)) (depth-tree (right-tree t))))))
  
(define (all-levels t)
  (define depth (depth-tree t))
  (define (loop i lst)
    (if (equal? i depth)
        lst
       (loop (+ i 1) (append lst (list (tree-level i t))))))
  (loop 0 '()))

;tree map
(define (tree-map f t)
  (if (empty-tree? t)
      '()
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

;tree -> list
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))


(define bst
  (make-tree 5
             (make-tree 2
                        (make-leaf 1)
                        (make-leaf 3))
             (make-tree 7
                        (make-leaf 1)
                        (make-leaf 2))))

;insert value in bst
(define (bst-insert val t)
  (if (empty-tree? t)
      (make-tree val empty-tree empty-tree)
      (if (< val (root-tree t))
          (make-tree (root-tree t) (bst-insert val (left-tree t)) (right-tree t))
          (make-tree (root-tree t) (left-tree t) (bst-insert val (right-tree t))))))

;tree-sort
(define (tree-sort lst)
  (tree->list (foldr bst-insert '() lst)))

;valid bst
(define (valid-bst? t)
  (cond
    [(empty-tree? t) #t]
    [(not (empty-tree? (left-tree t))) (and (> (root-tree t)
                                                (root-tree (left-tree t)))
                                            (valid-bst? (left-tree t)))]
    [(not (empty-tree? (right-tree t))) (and (<= (root-tree t)
                                                (root-tree (right-tree t)))
                                             (valid-bst? (right-tree t)))]
    [else #t]))
