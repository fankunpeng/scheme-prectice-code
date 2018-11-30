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

(define cons
  (lambda (x y)
    (lambda (m)
      (cond
       ((= m 0) x)
       ((= m 1) y)
       (else (error "error argument" m))))))

(define car
  (lambda (z)
    (z 0)))

(define cdr
  (lambda (z)
    (z 1)))

(car (cons 1 2))
(cdr (cons 1 2))
(define z (cons 3 4))
