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


(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      (cond
       ((null? (car l)) (occur* a (cdr l)))
       ((eq? (car (car l)) a)
        (+ (add1 (occur* a (cdr (car l))))
           (occur* a (cdr l))))
       (else (+ (occur* a (cdr (car l)))
                (occur* a (cdr l)))))))))

(occur* 'a '(a b c))
(occur* 'a '((a b) a c))
(occur* 'a '((a b) () a c))
(occur* 'a '((a b) (b a a c) a c))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else
      (cond
       ((null? (car l))
        (cons (car l)
              (subst* new old (cdr l))))
       ((eq? (car (car l)) old)
        (cons (cons new
                    (subst* new old(cdr (car l))))
              (subst* new old (cdr l))))
       (else (cons (cons (car (car l))
                         (subst* new old (cdr (car l))))
                   (subst* new old (cdr l)))))))))

(subst* 'e 'a '((a b) (b a a c) a c))
(subst* 'e 'a '((a b a c ) a (b a a c a) a c))
(subst* 'e 'a '((a a a) a () a c))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (cons (car l) (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else
      (cond
       ((null? (car l))
        (cons (car l) (insertL* new old (cdr l))))
       ((eq? (car (car l)) old)
        (cons
         (cons new
               (cons (car (car l))
                     (insertL* new old (cdr (car l)))))
         (insertL* new old (cdr l))))
       (else
        (cons
         (cons (car (car l))
               (insertL* new old (cdr (car l))))
         (insertL* new old (cdr l)))))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons old
                    (insertL* new old (cdr l)))))
       (else
        (cons (car l)
              (insertL* new old (cdr l))))))
     (else
      (cond
       ((null? (car l)) '())
       ((eq? (car (car l)) old)
        (cons
         (cons new
               (cons old
                     (insertL* new old (cdr (car l)))))
         (insertL* new old (cdr l))))
       (else
        (cons
         (cons (car (car l))
               (insertL* new old (cdr (car l))))
         (insertL* new old (cdr l)))))))))

(insertL* 'e 'a '((a (a) a a) a () a c))
(insertL* 'e 'a '((a (a (c a)) a a) a () a c))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
     (else
      (cond
       ((null? (car l)) #f)
       ((eq? (car (car l)) a) #t)
       (else (or (member* a (cdr l))
                  (member* a (cdr (car l))))))))))

(member* 'a '((a (a (c a)) a a) a () a c))
(member* 'a '((a (a (c a)) a a) a () a c))
(member* 'a '((e b e c) e (b e e c e) e c))
(member* 'a '(a))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (car l))))))

(leftmost '((e b e c) e (b e e c e) e c))
(leftmost '(a e c))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((and (atom? (car l1)) (atom? (car l2)))
      (cond
       ((and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))) #t)
       (else #f)))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(a e c) '(a e c))
(eqlist? '(b e c) '(a e c))
(eqlist? '((a) e c) '((a) e c))
(eqlist? '((a) e c) '(a e c))
(eqlist? '((a c) e c) '((a b) e c))
(eqlist? '(() e c) '((a b) e c))
(eqlist? '(() e c) '(() e c))

(define equal?
  (lambda (l1 l2)
    (cond
     ((and (atom? l1) (atom? l2)) (eq? l1 l2))
     ((or (atom? l1) (atom? l2)) #f)
     (else (eqlist? l1 l2)))))
(equal? '((1) e c) '(() e c))
(equal? '(() e c) '(() e c))
(equal? '(1 2 3 4) '(1 2 3 4))
(equal? '() 'a)
(equal? 'a 'a)

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? s (car l)) (rember s (cdr l)))
     (else (cons (car l) (rember s (cdr l)))))))

(rember '(a) '((a) b c))
(rember 'a '(a b c))
(rember '() '(a b c))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered? (car aexp))
           (numbered? (cdr (cdr aexp)))))
     ((eq? (car (cdr aexp)) '*)
      (and (numbered? (car aexp))
           (numbered? (cdr (cdr aexp)))))
     ((eq? (car (cdr aexp)) '^)
      (and (numbered? (car aexp))
           (numbered? (cdr (cdr aexp)))))
     (else #f))))


(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(numbered? '(1 + 2))
(numbered? '(1 + (1 + 2)))


(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((zero? (occur (car lat) (cdr lat)))
      (set? (cdr lat)))
     (else #f))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(occur '1 '(2 1 3))
(set? '(1 1 3 4))
(set? '(1 3 1 4))
(set? '(1 3 2 4))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) lat)
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(1 2 3 4 1 2 4))
(makeset '(1 2 3 4))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
                 (makeset (multiremember (car lat) (cdr lat))))))))

(multiremember '1 '(1 2 3 4 1 2 4))
(makeset '(1 2 3 4 1 2 4))
(makeset '(1 2 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset? (cdr set1) (cdr set2)))
     (else #f))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

(subset? '(1 2 5) '(1 2 3 4))
(subset? '(1 2 ) '(1 2 3 4))
(subset? '(1 2 ) '(1 2 3 4))


(define eqset?
  (lambda (set1 set2)
    (cond
     ((and (null? set1)
           (null? set2)) #t)
     ((or (null? set1)
          (null? set2)) #f)
     (else (member? (car set1) set2)
           (eqset? (cdr set1)
                   (remember (car set1)
                             set2))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((and (subset? set1 set2)
           (subset? set2 set1)) #t)
     (else #f))))

(define subset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(1 2 3 4) '(1 2 3 4))
(eqset? '(2 3 4 1) '(1 2 3 4))
(eqset? '(2 3 4 5) '(1 2 3 4))
(eqset? '(1 2 3) '(1 2 3 4))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((or (null? set1)
          (null? set2)) #f)
     ((member? (car set1) set2) #t)
     (else (intersect? (cdr set1)
                       set2)))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((or (null? set1)
          (null? set2)) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(intersect? '(1 2 3 4) '(1 2 3 4))
(intersect? '(2) '(1 2 3 4))
(intersect? '() '(1 2 3 4))
(intersect? '(5) '(1 2 3 4))

(define intersect
  (lambda (set1 set2)
    (cond
     ((intersect? set1 set2)
      (cond
       ((null? set1) '())
       ((member? (car set1) set2)
        (cons (car set1)
              (intersect (cdr set1) set2)))
       (else (intersect (cdr set1) set2))))
     (else '()))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '(2) '(1 2 3 4))
(intersect '() '(1 2 3 4))
(intersect '(3 5 7) '(1 2 3 4))
(intersect '(5 6 7) '(1 2 3 4))


(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))
(union '(5 6 7) '(1 2 3 4))
(union '(4 5 6 7) '(1 2 3 4))
(union '(4) '(1 2 3 4))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

(intersectall '((4) (1 2 3 4)))
(intersectall '((4) (2 4) (1 2 3 4)))
(intersectall '((4) (4) (1 2 3 4)))

(define a-pair?
  (lambda (x)
    (cond
     ((null? x) #f)
     ((atom? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(a-pair? '(1 2))
(a-pair? '(1 ))
(a-pair? '(1 2 '()))
(a-pair? '1)


(define first
  (lambda (p)
    (cond
     (else (car p)))))

(define second
  (lambda (p)
    (cond
     (else (car (cdr p))))))

(define build
  (lambda (s1 s2)
    (cond
     (else (cons s1
                 (cons s2 '()))))))

(build 'a 'b)
(build '() '())

(define third
  (lambda (l)
    (car (cdr (cdr l)))))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((a b) (c d)))
(fun? '((a b) (a d)))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (cons (car (cdr (car rel)))
                       (cons (car (car rel)) '()))
                 (revrel (cdr rel)))))))
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (car (cdr (car rel)))
                        (car (car rel)))
                 (revrel (cdr rel)))))))
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))
(revpair '(a b))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(revrel '((a b) (c d)))
(revrel '((a b) (c d) (e f)))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (second (car l))
                 (seconds (cdr l)))))))
(seconds '((a b) (c d) (e f)))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((a b) (c d) (e f)))
(fullfun? '((a b) (c d) (e b)))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
(one-to-one? '((a b) (c d) (e f)))
(one-to-one? '((a b) (c d) (e b)))

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l)
                 (rember-f test? a (cdr l)))))))
(rember-f = 5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(jelly) '((jelly) beans are good))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

((eq?-c 'hello) 'hello)
(define eq?-scala (eq?-c 'scala))
(eq?-scala 'scala)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a)
        (cdr l))
       (else (cons (car l)
                   ((rember-f test?) a (cdr l))))))))

((rember-f =) 5 '(6 2 5 3))
((rember-f eq?) 'jelly '(jelly beans are good))
((rember-f equal?) '(jelly) '((jelly) beans are good))
((rember-f eq?) 'eq? '(equal? eq? equan? eqlist? eqpair?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? old (car l))
        (cons new
              (cons (car l) (cdr l))))
       (else (cons (car l)
                   ((insertL-f test?) new old (cdr l))))))))
((insertL-f =) 1 6 '(1 2 3 4 5 6))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons (car l)
              (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR-f test?) new old (cdr l))))))))
((insertR-f =) 1 6 '(1 2 3 4 5 6))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old
          (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
        (seq new old ((insert-g seq) (cdr l))))
       (else (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

((insert-g seqL) 1 6 '(1 2 3 6 4 5 6))
((insert-g seqR) 1 6 '(1 2 3 6 4 5 6))


(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else (cons (car l)
		 (rember-f test? a (cdr l)))))))

