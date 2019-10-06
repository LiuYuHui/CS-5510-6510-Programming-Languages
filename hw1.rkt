#lang plai-typed

(define-type Tree
    [leaf (val : number)]
    [node (val : number)
          (left : Tree)
          (right : Tree)])

(define (sum [t : Tree]) : number
  (type-case Tree t
    [leaf (n) n]
    [node (v l r) (+ v
                     (+ (sum l)
                        (sum r)))]))

(test (sum (node 5 (leaf 6) (leaf 7))) 18)
(test (sum (leaf 0)) 0)

(define (negate [t : Tree]): Tree
  (type-case Tree t
    [leaf (n) (leaf (- 0 n))]
    [node (v l r) (node (- 0 v) (negate l) (negate r))]))

(test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))
(test (negate (leaf 5)) (leaf -5))

(define (contains? [t : Tree] [n : number]): boolean
   (type-case Tree t
     [leaf (v) (eq? v n)]
     [node (v l r) (or (eq? v n)
                       (or (contains? l n) (contains? r n)))]))

(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 10) #f)


(define (bigger-leaves? [t : Tree] [n : number]): boolean
  (type-case Tree t
    [leaf (v) (> v n)]
    [node (v l r) (and (bigger-leaves? l (+ v n))
                       (bigger-leaves? r (+ v n)))]))

(define (big-leaves? [t : Tree]) : boolean
  (bigger-leaves? t 0))

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)


(define (find-max [t : Tree]) : number
  (type-case Tree t
    [leaf (v) v]
    [node (v l r) (max v (max (find-max l) (find-max r)))]))

(test (find-max (node 8 (leaf 6) (leaf 7))) 8)


;;(define (sorted? [t : Tree]) : boolean
  ;; (type-case Tree t
   ;;  [leaf (v) #t]
    ;; [node (v l r) (and (> v (find-max l))
           ;;             (> (find-max r) v))]))

(define (sorted-helper [t : Tree] [min : number] [max : number]) : boolean
  (type-case Tree t
    [leaf (v) (and (<= min v) (<= v max))]
    [node (v l r) (and
                   (<= min v)
                   (<= v max)
                   (sorted-helper l min v)
                   (sorted-helper r v max))]))
(define (sorted? [t : Tree]) : boolean
  (sorted-helper t -inf.0 +inf.0))

(test (sorted? (node 6 (leaf 5) (leaf 7))) #t)

(test (sorted? (node 5 (node 2 (leaf 1) (leaf 3))
                     (node 6 (leaf 5 ) (leaf 7)))) #t)