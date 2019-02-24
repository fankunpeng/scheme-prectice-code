(define is-first
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? a (car lat))))))

(is-first 'a '(a b c))
(is-first 'a '())
(is-first 'a '(b a c))

(define two-in-a-row
  (lambda (lat)
    (cond
     ((null? lat) #f)
     ((is-first (car lat)
		(cdr lat)) #t)
     (else (two-in-a-row (cdr lat))))))

(two-in-a-row '(test a b test c))
(two-in-a-row '(test test a b c))


(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else (is-first-b? (car lat)
			(cdr lat))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (is-first-b? (car lat) (cdr lat))))))

(is-first-b? 'a '(a b c))
(is-first-b? 'a '(b a c))
(is-first-b? 'a '(b a a c))
(is-first-b? 'a '())

(two-in-a-row? '(a a b b))
(two-in-a-row? '(a b b))
(two-in-a-row? '(a b a))


