(define myfoldl (lambda (f defVal l) (if (null? l) defVal (myfoldl f (f defVal (car l)) (cdr l)))))

(define myfilter (lambda (p? l)
               (cond ((null? l) l)
                     ((p? (car l)) (cons (car l) (myfilter p? (cdr l))))
                     (else (myfilter p? (cdr l))))))

(define all? (lambda (l)
    (if (null? l) #f
        (null? (myfilter (lambda (x) (equal? #f x)) l)))))

;;;ако приемем, че сумата в непразно поддърво може да е нула,
;;;един връх е лист, ако няма деца или децата са му само празни списъци
(define isLeaf? (lambda (node)
   (or (null? (cdr node)) (all? (map null? (cdr node))))))

(define sum-leaves (lambda (tree)
     (cond ((null? tree) 0)
           ((isLeaf? tree) (car tree))
           (else (myfoldl + 0 (map sum-leaves (cdr tree)))))))

(define test1 (sum-leaves '(1 (0) (1 () (0)) () (9 (0) (0)) (9 (0) () ()))))