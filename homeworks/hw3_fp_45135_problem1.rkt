(define myfilter (lambda (p? l)
               (cond ((null? l) l)
                     ((p? (car l)) (cons (car l) (myfilter p? (cdr l))))
                     (else (myfilter p? (cdr l))))))

(define all? (lambda (l)
    (if (null? l) #f
        (null? (myfilter (lambda (x) (equal? #f x)) l)))))

(define zip (lambda (lhs rhs)
              (if (or (null? lhs) (null? rhs)) '()
                  (cons (cons (car lhs) (car rhs)) (zip (cdr lhs) (cdr rhs))))))

(define isPrefixOf? (lambda (sublist l)
  (and (<= (length sublist) (length l))
      (all? (map (lambda (x) (equal? (car x) (cdr x))) (zip sublist l))))))

(define takeWhile (lambda (p? l)
    (if (or (null? l) (not (p? l))) '()
        (cons (car l) (takeWhile p? (cdr l))))))

(define dropWhile (lambda (p? l)
    (if (or (null? l) (not (p? l))) l
        (dropWhile p? (cdr l)))))

;;;---------------------------------------------------------------------------

(define insert-before (lambda (a sublist l)
    (let* ((p? (lambda (x) (not (isPrefixOf? sublist x))))
           (first (takeWhile p? l))
           (second (dropWhile p? l)))
       ;;;ако sublist е празен винаги ще има съвпадение (на края)
      (if (and (null? second) (not (null? sublist))) #f
          (append first (cons a second))))))

(define test1 (insert-before  'a '(1 2 3) '(1 1 2 3 4)))
(define test2 (insert-before  'a '(1 9 3) '(1 1 2 3 4)))
(define test3 (insert-before  'a '() '(1 1 2 3 4)))
(define test4 (insert-before  'a '(1 2 3 4 5) '(1 1 2 3 4)))
(define test5 (insert-before  'b '(a) '(1 a 2 3 4)))