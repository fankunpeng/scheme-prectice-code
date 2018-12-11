(define make-rat
  (lambda (x y)
    (cons x y)))

(define numer
  (lambda (x)
    (car x)))

(define denom
  (lambda (x)
    (cdr x)))

(define add-rat
  (lambda (x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))

(add-rat (make-rat 1 3) (make-rat 1 6))

(define sub-rat
  (lambda (x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))))

(sub-rat (make-rat 1 3) (make-rat 1 6))

(define mul-rat
  (lambda (x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))))

(mul-rat (make-rat 1 3) (make-rat 1 6))

(define div-rat
  (lambda (x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x)))))

(div-rat (make-rat 1 2) (make-rat 1 4))

(define equal-rat
  (lambda (x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x)))))

(equal-rat (make-rat 1 2) (make-rat 2 4))

(define print-rat
  (lambda (x)
    (begin
      (display (numer x))
      (display "/")
      (display (denom x))
      (newline))))

(print-rat (make-rat 1 2))
(print-rat (div-rat (make-rat 1 2) (make-rat 1 4)))

(define %
  (lambda (x y)
    (cond
     ((< x y) x)
     (else (% (- x  y) y)))))
(% 3 1)
(% 1 3)
(% 6 8)
(% 8 6)

(define gcd
  (lambda (x y)
    (cond
     ((zero? (% x y)) y)
     (else (gcd y (% x y))))))

(gcd 3 1)
(gcd 1 3)
(gcd 6 8)
(gcd 8 6)

(define make-rat
  (lambda (x y)
    (begin
      (define g
        ((lambda (x y)
           (gcd x y)) x y))
      (cons (/ x g)
            (/ y g)))))

(define make-rat
  (lambda (x y)
    (begin
      (define g (gcd x y))
      (cons (/ x g)
            (/ y g)))))

(define make-rat
  (lambda (x y)
    (cons (/ x (gcd x y))
          (/ y (gcd x y)))))

(define make-rat
  (lambda (x y)
    (cond
     ((and (> x 0) (> y 0))
      (cons (/ x (gcd x y))
            (/ y (gcd x y))))
     ((and (> x 0) (< y 0))
      (cons (- (/ x (gcd x (- y))))
            (/ (- y) (gcd x (- y)))))
     ((and (< x 0) (> y 0))
      (cons (/ x (gcd (- x) y))
            (/ y (gcd (- x) y))))
     ((and (< x 0) (< y 0))
      (cons (/ (- x) (gcd (- x) (- y)))
            (/ (- y) (gcd (- x) (- y))))))))


(define make-rat
  (lambda (x y)
    (let ((g (gcd x y)))
      (cons (/ x g)
            (/ y g)))))

;; final
(define make-rat
  (lambda (x y)
    (let ((g (gcd (abs x) (abs y))))
      (cond
       ((> (* x y) 0)
        (cons (/ (abs x) g)
              (/ (abs y) g)))
       ((< (* x y) 0)
        (cons (- (/ (abs x) g))
              (/ (abs y) g)))))))

(make-rat 3 6)
(make-rat 8 6)
(make-rat (- 8) (- 6))
(print-rat (make-rat (- 8)  6))
(print-rat (make-rat 8  (- 6)))
(make-rat (- 8)  6)
(make-rat 8  (- 6))
(make-rat 8  6)

(add-rat (make-rat 1 (- 3))
         (make-rat 10 6))
(add-rat (make-rat 11 (- 3))
         (make-rat 10 6))

(define make-point
  (lambda (x y)
    (cons x y)))

(define x-point
  (lambda (p)
    (car p)))

(define y-point
  (lambda (p)
    (cdr p)))
(y-point (make-point 1 2))
(x-point (make-point 1 2))

(define print-point
  (lambda (p)
    (begin
      (display "(")
      (display (x-point p))
      (display ", ")
      (display (y-point p))
      (display ")")
      (newline))))

(print-point (make-point 1 2))

(define make-segment
  (lambda (start end)
    (cons start end)))

(make-segment (make-point 1 2)
              (make-point 3 4))

(define start-segment
  (lambda (segment)
    (car segment)))

(define end-segment
  (lambda (segment)
    (cdr segment)))

(start-segment (make-segment (make-point 1 2)
                             (make-point 3 4)))
(end-segment (make-segment (make-point 1 2)
                             (make-point 3 4)))

(define midpoint-segment
  (lambda (segment)
    (make-point (/ (+ (x-point (start-segment segment))
                      (x-point (end-segment segment))) 2)
                (/ (+ (y-point (start-segment segment))
                      (y-point (end-segment segment))) 2))))

(print-point (midpoint-segment (make-segment (make-point 1 2)
                                             (make-point 1 4))))
(print-point (midpoint-segment (make-segment (make-point 4 2)
                                             (make-point 1 4))))


(define reverse
  (lambda (l)
    (begin
      (define iter-reverse
        (lambda (l1 l2)
          (cond
           ((null? l1) l2)
           (else (iter-reverse (cdr l1)
                               (cons (car l1)
                                     l2))))))
      (iter-reverse l '()))))

(define iter-reverse
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     (else (iter-reverse (cdr l1)
                         (cons (car l1) l2))))))
(iter-reverse '(1 2 3) '())

(reverse '(1 2 3))

(define append
  (lambda (l1 l2)
    (cond 
     ((null? l1) l2)
     (else (cons (car l1)
                 (append (cdr l1) l2))))))
(append '(1 2 3) '(4 5 6))

(define list-ref
  (lambda (items n)
    (cond
     ((and (> n 0) (null? items)
           (error "error reference" "invalid index")))
     ((= n 1) (car items))
     (else (list-ref (cdr items)
                     (- n 1))))))

(list-ref '(1 2 3) 2)
(list-ref '(1 2 3) 3)


(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (+ 1 (length (cdr l)))))))

(length '(1 2 3 4))

(define last-pair
  (lambda (l)
    (cond
     ((null? (cdr l)) l)
     (else (last-pair (cdr l))))))

(define scale-list
  (lambda (l factor)
    (cond
     ((null? l) '())
     (else (cons (* (car l) factor)
                 (scale-list (cdr l) factor))))))

(scale-list '(1 2 3 4) 10)

(define map
  (lambda (proc items)
    (cond
     ((null? items) '())
     (else (cons (proc (car items))
                 (map proc (cdr items)))))))

(define map
  (lambda (proc items)
    (begin
      (define iter-map
        (lambda (proc l1 l2)
          (cond
           ((null? l1) l2)
           (else (iter-map proc (cdr l1) (cons (proc (car l1)) l2))))))
      (iter-map proc items '()))))


(map (lambda (a) (* a 10))
     '(1 2 3 4 ))

(define scale-list
  (lambda (items factor)
    (map (lambda (x) (* factor x))
         items)))

(scale-list '(1 2 3 4) 10)

(define part
  (lambda (old-l new-l povit)
    (cond
     ((null? old-l)
      (append old-l (cons povit new-l)))
     (else (cond
            ((> (car old-l) povit)
             (part (cdr old-l) (cons (car old-l) new-l) povit))
            (else (cons (car old-l)
                        (part (cdr old-l) new-l povit))))))))


(part '(1 3 4 2) '() 2)

(define quick-sort
  (lambda (l)
    (begin
      (define less-than
        (lambda (items povit)
          (cond
           ((null? items) '())
           ((< (car items) povit)
            (cons (car items)
                  (less-than (cdr items) povit)))
           (else (less-than (cdr items) povit)))))
      (define bigger-than
        (lambda (items povit)
          (cond
           ((null? items) '())
           ((> (car items) povit)
            (cons (car items)
                  (bigger-than (cdr items) povit)))
           (else (bigger-than (cdr items) povit)))))
      (define join
        (lambda (l1 l2 povit)
          (append l1 (cons povit l2))))
      (cond
       ((null? l) '())
       (cons (join (quick-sort (bigger-than (cdr l) (car l)) )
                   (quick-sort (less-than (cdr l) (car l)))
                   (car l)))))))

(quick-sort '(1 3 2 4))
(quick-sort '(1 3 2 4))
(quick-sort '(3 2 72 4235 5135 52135 5 63 43134 2351 51 164  27 ))
(quick-sort '(3 3 2 72 4235 5135 52135 5 63 43134 2351 51 164  27 ))

(define squre-list
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (* (car l) (car l))
                 (squre-list (cdr l)))))))

(squre-list '(1 2 3 4))

(define squre-list
  (lambda (l)
    (map (lambda (i) (* i i)) l)))

(squre-list '(1 2 3 4))

(define for-each
  (lambda (proc items)
    (cond
     ((null? items) '())
     (else
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))))

(for-each (lambda (x) (begin
                        (display x)
                        (newline))) '(1 2 3 4))

(for-each (lambda (x) (begin
                        (display (* x x))
                        (newline))) '(1 2 3 4))

(define count-leaves
  (lambda (x)
    (cond
     ((null? x) 0)
     ((not (pair? x)) 1)
     (else (+ (count-leaves (car x))
              (count-leaves (cdr x)))))))

(count-leaves '((1 2) 3 4))

(define deep-reverse
  (lambda (x)
    (cond
     ((null? x) '())
     ((not (pair? x)) x)
     (else (cons (deep-reverse (cdr x))
                 (deep-reverse (car x)))))))
(deep-reverse '((1 2) (3 4)))

(define iter-deep-reverse
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     ((pair? (car l1))
      (iter-deep-reverse (cdr l1)
                    (cons (iter-deep-reverse (car l1) '())
                          l2)))
     (else (iter-deep-reverse (cdr l1) (cons (car l1) l2))))))
(define deep-reverse
  (lambda (l)
    (iter-deep-reverse l '())))

(deep-reverse '((1 2) (3 4)) )
(deep-reverse '((1 2) (3 4 (1 2 3 4 (5 6 7 (8 9))))) )

(define fringe
  (lambda (l)
    (cond
     ((null? l) '())
     ((pair? (car l))
      (cond
       ((null? (fringe (car l)))
        (fringe (cdr l)))
       (else (cons (fringe (car l))
                   (fringe (cdr l))))))
     (else (cons (car l)
                 (fringe (cdr l)))))))


(define fringe
  (lambda (l)
    (cond
     ((null? l) '())
     ((or (pair? (car l))
           (null? (car l)))
      (append (fringe (car l))
              (fringe (cdr l))))
     (else (cons (car l)
                 (fringe (cdr l)))))))

(fringe '((1 2) (3 4)) )
(define l '((1 2) (3 4)))
(fringe (list l l (list l (list l l)) ))

(define scale-tree
  (lambda (tree factor)
    (cond
     ((null? tree) '())
     ((pair? (car tree))
      (cons (scale-tree (car tree) factor)
            (scale-tree (cdr tree) factor)))
     (else (cons (* factor (car tree))
                 (scale-tree (cdr tree) factor))))))


(define map
  (lambda (proc tree)
    (cond
     ((null? tree) '())
     ((pair? (car tree))
      (cons (map proc (car tree))
            (map proc (cdr tree))))
     (else (cons (proc (car tree))
                 (map proc (cdr tree)))))))

(define scale-tree
  (lambda (tree factor)
    (map (lambda (x) (* factor x)) tree)))

(map (lambda (x) (* x 10)) '(1 2 (1 2)))

(scale-tree '(1 2 3 (1 2) 3) 10)

(define squre-tree
  (lambda (tree)
    (cond
     ((null? tree) '())
     ((pair? (car tree))
      (cons (squre-tree (car tree))
            (squre-tree (cdr tree))))
     (else (cons (* (car tree) (car tree))
                 (squre-tree (cdr tree)))))))
(define squre-tree
  (lambda (tree)
    (map (lambda (x) (* x x)) tree )))

(squre-tree '(1 2 3 (1 2) 3))
(squre-tree '(1 (2 3) (1 2) 3))

(define (subsets s)
  (if (null? s) '(())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define subsets
  (lambda (s)
    (cond
     ((null? s) s)
     (else (let ((rest (subsets (cdr s))))
             (append rest (map (lambda (x) (list (car s) x)) rest)))))))

(define subsets
  (lambda (s)
    (cond
     ((null? s) '(()))
     (else (append (subsets (cdr s))
                   (map (lambda (x) (cons (car s) x))
                        (subsets (cdr s))))))))

(append '(1 2) '(3 4))
(subsets '(1))
(subsets '(1 2))
(subsets '(1 2 3))
(subsets '(1 2 3 4 5 6 7 8 9 10))


(map (lambda (x) (cons 1 x))
     '(() (3)))
