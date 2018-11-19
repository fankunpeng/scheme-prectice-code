(define atom?
  (lambda (x)
    (and  (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (lst)
    (cond
     ((null? lst) #t)
     ((atom? (car lst)) (lat? (cdr lst)))
     (else #f))))

(lat? '())
(lat? '(1 2 3))
(lat? '(() ()))
(lat? '(hello world))
(lat? '((hello world) hello every one))

(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((eq? (car l) x) #t)
     (else (member? x (cdr l))))))

(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     (else (or (eq? (car l) a)
               (member? a (cdr l)))))))

(member? 'a '(a b c))
(member? 'a '( b c))
(member? 'a '())
(member? 'a '(b a c))

(define remember
  (lambda (x l)
    (cond
     ((null? l ) '())
     ((eq? x (car l)) (cdr l))
     (else (cons (car l) (remember x (cdr l)))))))

(define remember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? a (car lat)) (cdr lat))
            (else (cons (car lat) (remember a (cdr lat)))))))))

(remember 0 '(0))
(remember 0 '(0 1 0 1))
(remember 0 '(1 2 3))
(remember 1 '(2 1 3 1 4))
(remember 'and '(bacon lettuce and tomato))
(cons 'bacon (cons 'lettuce (cons 'tomato '())))


(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts '((apple peach pumpkin)
         (plum pear cherry)
         (grape raisin pea)
         (bean carrot eggplant)))

(firsts '((1 2) (3 4) (5 6)))
(firsts '())

(firsts '((a b) (c d) (e f)))
(cons 'a (cons 'c (cons 'e '())))


(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'e 'd '(a b c d f))
(insertR 'e 'd '())

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'e 'd '(a b c d f))
(insertL 'e 'a '(a b c d f))
(insertL 'e 'a '())

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? o1 (car lat)) (eq? o2 (car lat)) ) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((eq? o1 (car lat)) (cons new (cdr lat)))
     ((eq? o2 (car lat)) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

(subst2 'e 'a 'b '(b a c d))
(subst2 'e 'a 'b '(c a b d))

(define multiremember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multiremember a (cdr lat)))
     (else (cons (car lat)
                 (multiremember a (cdr lat)))))))

(multiremember 'e '(c e a e b e d))
(multiremember 'e '())


(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons (car lat)
                                (cons new
                                      (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))

(multiinsertR 'e 'd '(a d b c d d '() f  f))
(multiinsertR 'e 'm '(a d b c d d '() f  f))
(multiinsertR 'e 'm '('()))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'e 'd '(a b c d d d))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)  (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst 'e 'd '(a b c d d d))

(define add1
  (lambda (n)
    (1+ n)))

(define sub1
  (lambda (n)
    (1- n)))

(add1 1)
(sub1 1)

(define +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (+ n (sub1 m)))))))

(define -
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (- n (sub1 m)))))))

(define *
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (* n (sub1 m)))))))

;; 大数在前
(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(define /
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (/ (- n m) m))))))

(define %
  (lambda (n m)
    (cond
     ((< n m) n)
     (else (% (- n m) m)))))

(+ 3 4)
(- 4 3)
(* 3 4)
(< 3 4)
(< 4 3)
(> 3 4)
(> 4 3)
(> 3 3)
(< 3 3)
(/ 6 3)
(/ 3 6)
(/ 15 3)
(% 15 3)
(% 2 3)
(% 4 1)
(% 4 3)

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (* n (^ n (sub1 m)))))))
(^ 3 4)
(^ 2 10)
(^ 2 0)

(define =
  (lambda (n m)
    (cond
     ((and (zero? m) (zero? n)) #t)
     ((or (zero? m) (zero? n)) #f)
     (else (= (sub1 m) (sub1 n))))))
(= 1 2)
(= 1 1)

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))
(length '())
(length '(1 2 3))


(define pick
  (lambda (n lat)
    (cond
     ((null? lat) "error")
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))
(pick 1 '(1 2 3))
(pick 2 '(1 2 3))
(pick 4 '(1 2 3))

(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 1 '(1 2 3))
(rempick 2 '(1 2 3))
(rempick 3 '(1 2 3))

(define no-numbers
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-numbers (cdr lat)))
     (else (cons (car lat) (no-numbers (cdr lat)))))))
(no-numbers '(1 hello world))
(no-numbers '(1 hello 2 world 3 effitive))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '() )
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(all-nums '(1 hello 2 world 3 effitive))
(all-nums '(hello world effitive))

(define eqan?
  (lambda (m n)
    (cond
     ((and (atom? m) (atom? n)) (eq? m n))
     ((and (number? m) (number? n)) (= m n))
     (else #f))))


(eqan? 1 2)
(eqan? 1 1)
(eqan? 'e 'f)
(eqan? 'e 'e)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(occur 'a '(a b c d a b c))
(occur 'a '(b c d))

(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))
(one? 1)
(one? 2)
(one? 0)


(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(rempick 4 '(1 2 3 4))

(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((atom? (car lat))
      (cond
       ((eq? (car lat) a)
        (rember* a (cdr lat)))
       (else (cons (car lat)
                   (rember* a (cdr lat))))))
     (else  (cons (rember* a (car lat))
                  (rember* a (cdr lat)))))))

(rember* 'a '((a) b a (a c) d))
(rember* 'a '(() b a (a c) (c a) d))

