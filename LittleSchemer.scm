#lang scheme
(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f )
      (else (or ( eq? ( car lat) a)
                (member? a ( cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              (( equal? ( car lat) a) ( cdr lat))
              (else (cons ( car lat)
                          (rember a
                             ( cdr lat)))))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()) )
      (else ( cons ( car ( car l))
                   (firsts ( cdr l)))))))

(define oinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ( ( eq? ( car lat) old)
                ( cons old
                       ( cons new ( cdr lat))))
              (else ( cons ( car lat)
                           ( oinsertR new old
                                     ( cdr lat)))))))))
(define oinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((equal? ( car lat) old)
               ( cons new
                      ( cons old ( cdr lat))))
              (else ( cons ( car lat)
                           ( oinsertL new old
                                     ( cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((or ( eq? ( car lat) o1) ( eq? ( car lat) o2))
               ( cons new ( cdr lat)))
              (else (cons (car lat)
                          ( subst2 new o1 o2
                                   ( cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((equal? ( car lat) a)
          ( multirember a ( cdr lat)))
         (else ( cons ( car lat)
                      ( multirember a
                                    ( cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ( ( eq? ( car lat) old)
           ( cons ( car lat)
                  ( cons new
                         ( multiinsertR new old
                                        ( cdr lat)))))
         (else ( cons ( car lat)
                      ( multiinsertR new old
                                     ( cdr lat)))))))))

(define multiinsertL
  (lambda ( new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         (( eq? ( car lat) old)
          ( cons new
                 ( cons old
                        ( multiinsertL new old
                                       ( cdr lat)))))
         (else ( cons ( car lat)
                      ( multiinsertL new old
                                     ( cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? ( car lat) old)
               ( cons new
                      ( multisubst new old
                                   ( cdr lat))))
              (else (cons (car lat)
                          ( multisubst new old
                                       ( cdr lat)))))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda ( n m)
    (cond
      ((zero? m) n)
      (else ( add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else(sub1 (o- n(sub1 m)))))))

(define addtup
  (lambda ( tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup(cdr tup)))))))

(define *
  (lambda ( n m)
    (cond
      ((zero? m) 0)
      (else (+ n ( * n (sub1 m)))))))

(define tup+
  (lambda ( tup1 tup2)
    (cond
      ((null? tup1 ) tup2)
      ((null? tup2) tup1 )
      (else
       ( cons ( + ( car tup1) ( car tup2))
              (tup+
               ( cdr tup1) ( cdr tup2)))))))

(define >
  (lambda ( n m)
    (cond
      ((zero? n) #f )
      ((zero? m) #t )
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda ( n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t )
      (else ( < (sub1 n) (sub1 m))))))

(define =
  (lambda ( n m)
    (cond
      ((> n m) #f)
      ((< n m) #f )
      (else #t ))) )
(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else(* n (expt n (sub1 m)))))))

(define quotient
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else ( add1 ( quotient ( - n m) m))))))

(define length
  (lambda ( lat)
    (cond
      ((null? lat) 0)
      (else ( add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) ( car lat))
      (else (pick ( sub1 n) ( cdr lat))))))

(define rempick
  (lambda ( n lat)
    (cond
      ((zero? (sub1 n)) ( cdr lat))
      (else ( cons ( car lat)
                   (rempick (sub1 n)
                            ( cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? ( car lat))
               (no-nums ( cdr lat)))
              (else ( cons ( car lat)
                           (no-nums
                            ( cdr lat)))))))))

(define all-nums
  (lambda ( lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? ( car lat))
          ( cons ( car lat)
                 ( all-nums (cdr lat))))
         (else ( all-nums ( cdr lat))))))))

(define eqan?
  (lambda ( a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else ( eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? ( car lat) a)
          ( add1 ( occur a ( cdr lat))))
         (else ( occur a ( cdr lat))))))))

(define one?
  (lambda (n)
    (= n 1)))

(define removepick (lambda (n lat)
                (cond
                  ((one? n) ( cdr lat))
                  ( else ( cons ( car lat)
                                removepick (sub1 n)
                                ( cdr lat))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      (( atom? (car l))
       (cond
         ((eq? ( car l) a)
          ( rember* a ( cdr l)))
         (else ( cons ( car l)
                      ( rember* a (cdr l))))))
      (else (cons ( rember* a ( car l))
                  ( rember* a ( cdr l)))))))

"from portable linux system- to check later"
 (define insertR*
  (lambda(new old l)
    (cond
      ((null? l)(quote()))
      ((atom? (car l))
       (cond
         (eq? (car l) old)
         (cons old
               (cons new
                     (insertR*(new old (cdr l)))))
         (else (cons (car l)
                     (insertR*(new old
                                   (cdr l)))))))
      (else cons (insertR*(new old
                               (car l))
                          (insertR*(new old (cdr l))))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l)0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1(occur*(a (cdr l)))))
         (else (occur*(a (cdr l))))))
       (else (o+ (occur* a (car l))
                 (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
                (subst* new old (cdr l))))
         (else (cons (car l)
                     (subst* new old
                             (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (caar l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old
                      (insertL* new old
                                (cdr l)))))
         (else (cons (car l)
                     (insertL* new old
                               (cdr l))))))
      (else (cons (insertL* new old
                            (car l))
                  (insertL* new old
                            (cdr l)))))))

(define member*
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
     (or (eq? a (car l))
         (member* a (cdr l))))
    (else (or (member* a (car l))
              (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2))#t)
      (else (and
             (eqan? (car l1)(car l2))(eqlist2? (cdr l1)(cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2))#t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1)(cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)(atom? s2))(eqan? s1 s2))
       ((or(atom? s1)(atom? s2))#f)
       (else(eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered?
             (car(cdr(cdr aexp)))))))))

(define ovalue
  (lambda (nexp)
    (cond
      ((atom? (nexp))(nexp))
      ((eq? (operator nexp) (quote +)) (o+ (ovalue (1st-sub-exp nexp))(ovalue (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *)) (* (ovalue (1st-sub-exp nexp)) (ovalue (2nd-sub-exp nexp))))
      (else (exp (ovalue 1st-sub-exp) (ovalue 2nd-sub-exp))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))
(define 2nd-sub-exp
  (lambda (aexp)
    (car(cdr (cdr aexp)))))
(define operator
  (lambda (aexp)
    (car (cdr aexp))))
(define wzero?
  (lambda (l)
    (null? l)))
(define wadd1
  (lambda (l)
    (cons l (quote ()))))
(define wsub1
  (lambda (l)
    (cdr l)))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cons (car lat)
                  (makeset
                   (multirember (car lat)
                                (cdr lat))))))))

(define osubset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (osubset?(cdr set1) set2))
       (else #f))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1)set2)
            (subset? (cdr set1)set2))))))

(define eqset?
  (lambda (set1 set2)
      (and (subset? set1 set2)
      (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or(member? (car set1) set2)
               (intersect?
                (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  ( union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car(cdr p))))
(define third
  (lambda (l)
    (car(cdr(cdr l)))))
(define build
  (lambda (s1 s2)
    (cons s1 ( cons s2 (quote ())))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()) )
      (else ( cons (second (car l))
                   (seconds ( cdr l)))))))

(define one-to-one?
  (lambda (fun)
    (fun?(revrel fun))))

(define cookies
  (lambda ()
    ("bake"
     (quote (350 degrees))
     (quote (12 minutes))
     ("mix"
      (quote(walnuts 1 cup))
      (quote(chocolate-chips 16 ounces))
      ("mix"
       ("mix"
        (quote(flour 2 cups))
        (quote (oatmeal 2 cups))
        (quote (salt .5 teaspoon))
        (quote (baking-powder 1 teaspoon))
        (quote (baking-soda 1 teaspoon)))
       ("mix"
        (quote (eggs 2 large))
        (quote (vanilla 1 teaspoon))
        ("cream"
         (quote (butter 1 cup))
         (quote (sugar 2 cups)))))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
    (cond
      ((null? l) (quote ()))
      (( test? ( car l) a) ( cdr l))
      (else (cons ( car l)((rember-f test?) a ( cdr l))))))))

(define eq?-c 
  (lambda ( a ) 
    (lambda ( x ) 
      ( eq? x a ))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((test? ( car l) old)
         ( cons new( cons old ( cdr l))))
         (else ( cons ( car l)
                ((insertL-f test?) new old
                 ( cdr l))))))))

(define insertR-f
  (lambda(test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        (( test? ( car l) old)
         ( cons old( cons new ( cdr l))))
        (else ( cons ( car l)
                ((insertR-f test?) new old
                 ( cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqS
  (lambda (new old l)
    (cons new l)))
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? ( car l) old)
         ( seq new old ( cdr l)))
         (else ( cons ( car l)
                ((insertL-f seq) new old
                 ( cdr l))))))))
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))
(define insertR (insert-g seqR))

(define subst (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) o+)
      ((eq? x (quote x)) *)
      (else expt))))

(define Oldvalue
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function
         (operator nexp))
        (Oldvalue(1st-sub-exp nexp))
        (Oldvalue(2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) a)
         ((multirember-f test?) a
            (cdr lat)))
        (else (cons (car lat)
               ((multirember-f test?) a
                 (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

(define multiremberT
  (lambda (test? lat)
    (cond
        ((null? lat) (quote()))
        ((test? (car lat))
         (multiremberT 
            test? (cdr lat)))
        (else (cons (car lat)
               (multiremberT test?
                 (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote()))
      ((eq?(car lat) oldL)
       (cons new
             (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq?(car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else(cons (car lat)
                 (multiinsertLR new oldL oldR
                               (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col(quote()) 0 0))
      ((eq?(car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col(cons new
                                     (cons oldL newlat))
                               (add1 L) R))))
      ((eq?(car lat) oldR)
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col(cons oldR (cons new newlat))
                           L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col(cons(car lat) newlat) L R)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote()) 0 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
            (lambda (newl p s)
              (col (cons(car l) newl)
                   (*(car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (lambda(newl p s)
                                 (col newl
                                      p(o+(car l) s)))))))
      (else evens-only*&co (car l)
            (lambda (al ap as)
              (evens-only*&co (cdr l)
                 (lambda(dl dp ds)
                    (col (cons al dl)
                        (*  ap dp)
                        (o+ as ds)))))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat ))
      (else (eq? sorn a)))))

(define eternity
  (lambda (x)
    eternity x))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
 eternity))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
 eternity)))

(define length0
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l)))))))
"make this (length 1) with the up 2 definition"
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length0 (cdr l))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
           ((lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l)))))))))))

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else(add1(length (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length eternity)
               (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((mk-length mk-length)
               (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1
              ((lambda (x)
                 ((mk-length mk-length) x))
               (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else
           (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
"applicative-order Y combinator is make length extracted from the above length function"
(lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le(lambda (x)
           ((mk-length mk-length) x))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                     (first entry)
                     (second entry)
                     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq?(car names) name)
       (car values))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table)(table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

"the function value, with all the functions it uses is an interpreter"
(define value
  (lambda (e)
    (meaning e(quote()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
           *lambda)
         ((eq? (car e) (quote cond))
          (quote *cond))
         (else (*application))))
      (else (*application)))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else(build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x(quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

"functions: primitives and non-primitives represented as such"
"(primitive primitive-name) & (non-primitive (table formals body))"
"the non-primitives list (cdr) is called a closure record"

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply?
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr(first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals)(second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero?(first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive)) #t)
      (else #f))))

(define new-entry build)

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))