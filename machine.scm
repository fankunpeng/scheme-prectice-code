(define gcd
  (lambda (a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(gcd 2 4)
(gcd 12 14)
(gcd 1 5)


(define remainder
  (lambda (a b)
    (if (< a b)
        a
        (remainder (- a b) b))))

(remainder 6 2)
(remainder 1 2)
