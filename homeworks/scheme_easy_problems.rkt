;;;Имплементирайте функцията (digit-list n), която приема число n и връща списък от цифрите му
(define (digit-list n)
  (letrec ((help 
                 (lambda (x) 
                   (let ((temp (mod x 10)))
                              (if(= x 0)
                               '()
                              (append (help (/ (- x temp) 10)) (list temp) ) )))))
         (if(= n 0)
            (list 0)
            (help n))))
          
;;;Имплементирайте функциите (index-of lst val) и (last-index-of lst val).
;За сравняване използвайте предиката (equal? val1 val2).
(define (index-of lst val)
  (let ((pred (lambda (x)
                (if(= -1 x)
                      -1
                      (+ x 1)))))
  (letrec ((help (lambda (llist)
                   (cond ((null? llist) -1)
                         ((equal? (car llist) val) 0)
                         ( else    (pred(help (cdr llist))))))))
                        (help lst))))



(define (last-index-of lst val)
  (let ((pred (lambda (x)
                (if(= -1 x)
                      -1
                      (+ x 1))))
        (max (lambda (lhs rhs)
               (if(> lhs rhs)
                     lhs
                     rhs))))
  (letrec ((help (lambda (llist)
                   (cond ((null? llist) -1)
                         ((equal? (car llist) val) (max 0 (pred(help (cdr llist)))))
                         (else    (pred(help (cdr llist))))))))
                    (help lst))))
                  
;;;Имплементирайте функцията (remove lst val), която премахва всички срещания на val в списъка lst.
(define (remove-w lst val)
  (letrec ((help (lambda (l) (cond ((null? l) '())
                                   ((= (car l) val) (help (cadr l)))
                                   (else (cons (car l) (help (cdr l))))))))
   (help lst)))

;;;Имплементирайте функциите (distinct? lst) и (distinct lst)
(define (distinct? lst)
  (cond ((null? lst) #t)
        ((member (car lst) (cdr lst)) #f)
        (else (distinct? (cdr lst)))))
      
(define (distinct lst)
  (letrec ((help (lambda (l res)
                   (cond ((null? l) res)
                         ((member (car l) res) (help (cdr l) res))
                         (else (help (cdr l) (append res (list (car l))))))))) 
   (help lst '() )
   ))

;;;Имплементирайте функцията (histogram lst), която получава списък от числа и връща списък от наредени двойки
;<число, брой срещания> - по една за всеки различен елемент на списъка.
(define (histogram lst)
  (letrec ((h (lambda (l val)
                (cond ((null? l) (list (list val 1)))
                      ((equal? (car (car l)) val) 
                               (cons (list val  (+ (cadr (car l)) 1)) (cdr l)))
                               (else (cons (car l)  (h (cdr l) val)))))))
   (if(null? lst)
             '()
              (h (histogram (cdr lst)) (car lst)))))
                       
(define (histogramm lst)
   (if(null? lst)
             '()
             ((lambda (l val)
               (letrec ((h (lambda (x)
                (cond ((null? x) (list (list val 1)))
                      ((equal? (car (car x)) val) 
                               (cons (list val  (+ (cadr (car x)) 1)) (cdr x)))
                             (else (cons (car x)  (h (cdr x) val)))))))
                           (h l)))
              (histogramm (cdr lst)) (car lst) )))                       
                       

(define (member? lst x)
  (cond ((null? lst) #f)
       ((equal? (car lst) x) #t)
        (else (member? (cdr lst) x))))
      
;(define (member? lst x)
 ; (letrec ((h (lambda (l) 
  ;      (cond ((null? l) #f)
   ;     ((equal? (car l) x) #t)
    ;    (else (h (cdr l)))))))
     ; (h lst)))
      
      
;Имплементирайте функцията (range n), която връща списък с числата от 0 до n.
(define (range n)
  (letrec ((h (lambda (cnt)
                (if(> cnt n)
                      '() 
                      (cons cnt (h (+ cnt 1)))))))
        (h 0)))
      
;Имплементирайте функциите (take n lst) и (drop n lst).
(define (take n lst)
  (if(or (< n 1)(null? lst))
         '()
         (cons (car lst) (take (- n 1) (cdr lst)))))
       
(define (drop n lst)
  (if(or (< n 1)(null? lst))
         lst
         (drop (- n 1) (cdr lst))))
       

(define (get pred lst default-value)
  (apply pred (cons default-value lst)))
                            
                            
;Имплементирайте функцията (sort lst), която сортира списъка lst чрез вмъкване.
(define (insert-sort-list lst element pred)
  (letrec ((h(lambda (l)
               (if(or(null? l)(not (pred (car l) element)))
                            (cons element l)
                            (cons (car l) (h (cdr l)))))))
          (h lst)))


(define (insertion-sort lst pred)
  (letrec ((h (lambda (l)
                (if(null? l)
                          '()
                        (insert-sort-list (h (cdr l)) (car l) pred)))))
            (h lst)))


;Имплементирайте функцията (group-by criteria lst), която групира елементите на списъка lst според резултата от функцията criteria
;(group-by (lambda (x) (mod x 3)) (range 6))
; => ((0 (0 3)) (1 (1 4)) (2 (2 5)))
;(group-by even? (range 6))
; => ((#t (0 2 4)) (#f (1 3 5)))
(define (group-by criteria lst)
  (let ( (f1 (lambda (key value res)
             (letrec ((f2 (lambda (l)
                          (cond ((null? l) (list (list key (list value))))
                                ((= key (car (car l)))
                                    (cons (cons key (list (cons value (cadr(car l)))))(cdr l)))
                                (else (cons (car l) (f2 (cdr l))))))))
           (f2 res)))))
  (letrec ((f3 (lambda (l)
                (if(null? l)
                        '()
                        (f1 (criteria (car l)) (car l) (f3 (cdr l)))))))
                (f3 lst))))

;Имплементирайте функцията (flatten lst), която премахва влаганията от даден списък
;(flatten '(1 2 (3 (4) 5) (((6))))) ; => (1 2 3 4 5 6)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (flatten lst)
  (cond ((null? lst) '())
        ((atom? lst) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

;Имплементирайте функцията (chunk size lst), която разделя елементите на списъка lst в групи с големина size
;(chunk 4 (range 10)) ; => ((0 1 2 3) (4 5 6 7) (8 9))
;(chunk 2 (range 4)) ; => ((0 1) (2 3))
(define (chunk n lst)
  (if(null? lst)
        '()
  (cons (take n lst) (chunk n (drop n lst)))))

;Имплементирайте функцията (flip fn), която обръща реда на аргументите на функцията fn незвисимо от броя им.
;(define list^ (flip list))
;(list^ 1 2 3) ; => (3 2 1)
;(define (reverse lst)
 ; (if(null? lst)
  ;          '()
   ;         (append (reverse (cdr lst)) (list (car lst)))))

(define (flip func)
  (lambda x (func (reverse x))))

;Имплементирайте функцията (juxt . fns) (от англ. juxtaposition - съпоставяне). Резултатът от (juxt f g h ...)
;e функция с променлив брой аргументи, която връща списък с резултатите от прилагането
;на всяка една от функциите f, g, h ... въргу тези аргументи.
;(define f (juxt inc dec square double)) ; (f x) = (list (inc x) (dec x) (square x) (double x))
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
(define (square x)
  (* x x))
(define (double x)
  (* x 2))

(define (juxt . args)
        (lambda (x)
          (map (lambda (f) (f x)) args))) 

    

;Да се напише функция (meetTwice? f g a b), която проверява дали в целочисления интервал [a; b] съществуват две различни цели числа x и y такива,
;че f(x) = g(x) и f(y) = g(y).
(define (meetTwice? f g a b)
  (letrec ((down (lambda (cnt)
                   (if(or (< cnt a) (= (f cnt) (g cnt)))
                         cnt
                         (down (dec cnt)))))
           (up (lambda (cnt)
                   (if(or (> cnt b) (= (f cnt) (g cnt)))
                          cnt
                          (up (inc cnt))))))
            (if (>= a b)
                #f
                (> (down b) (up a)))))

;Да се напише функция (maxDuplicate ll), която по списък от списъци от цели числа ll намира най-­голямото от тези числа, които
;се повтарят в рамките на списъка, в който се срещат. Ако в нито един списък няма повтарящи се числа, функцията да връща #f.
;(maxDuplicate '((1 2 3) (-­4 -­5 -6) ())) → #f
;(maxDuplicate '((1 2 3 2) (-­4 -­4) (5))) → 2
(define (my-filter f s)
  (cond ((null? s) '())
        ((f (car s)) (cons (car s) (my-filter f (cdr s))))
        (else (my-filter f (cdr s)))))




(define (maxDuplicate lst)
  (letrec ((h (lambda (l) (cond ((null? l) '()) ((member? (cdr l) (car l)) (cons (car l) (h (cdr l)))) (else (h (cdr l)))))))
    (let ((x (flatten (map h lst))))
      (if(null? x)
         #f
         (apply max x)))))
    

;Да се напише функция (checkMatrix? m k), която проверява дали e вярно, че няма ред в дадената матрица m, която да се състои само от делители на k.
;Пример: (checkMatrix ‘((1 2 6) (3 8 9) (6 11 12)) 12) → #f
;Пример: (checkMatrix ‘((1 2 7) (3 8 9) (6 11 12)) 12) → #t


(define (checkMatrix? matrix k)
  (if(null? matrix)
        #t
  (let ((len (length (car matrix))))
    (equal? '()
    (filter (lambda (sublist) (= len (length sublist)))
    (map (lambda (row) (filter (lambda (elem) (= 0 (remainder k elem))) row)) matrix))))))


;Да се напише функция (longestAscending­ l), която намира възходящо сортиран подсписък на списъка от числа
;l с максимална дължина. Ако съществуват няколко такива подсписъка, функцията да върне първия отдясно наляво.
;Упътване: Реализирайте помощна функция, която намира най-дългия възходящо сортиран префикс на даден списък.
;Пример: (longestAscending­ ‘(5 3 8 6 4 2 6 7 1)) → (2 6 7)
;Пример: (longestAscending­ ‘(6 5 4 3 2 1)) → (1)
(define (longestAscending l)
  (letrec ((pre-sort (lambda (lst) (if(or (null? (cdr lst)) (> (car lst) (cadr lst))) (list (car lst)) (cons (car lst) (pre-sort (cdr lst)))))))
  (if (null? l)
      '()
      (let ((x (longestAscending (cdr l)))
            (y (pre-sort l)))
        (if(> (length y) (length x))
           y
           x)))))

(define (hailstone n)
  (cond ((= n 1) (list 1))
        ((= (remainder n 2) 1) (cons n (hailstone (+ (* 3 n) 1))))
        (else (cons n (hailstone (quotient n 2))))))

(define (sum l)
  (get + l 0))

;Напишете предикат, който приема списък от списъци от числа и връща #t точно когато някой от
;списъците съдържа елемент, който е равен на сумата
;на елементите на някой от останалите списъци. Приемете, че празните списъци имат сума на елементите 0.
;funky? '((0 1 -1)
 ;         (1 1)
  ;        (2 3))) ; => #t; 2 = 1 + 1
;(funky? '((1 -1)
 ;         (1 1)
  ;        (1 3))) ; => #f
(define (funky? l)
  (let ((ll (flatten l)))
  (not (null? (filter (lambda (x)(member? ll x))(map sum l))))))

(define (rep l n)
  (if(>= 0 n)
     '()
     (cons l (rep l (- n 1)))))

;Напишете функция (transpose m), която транспонира матрицата m.
(define (transpose m)
  (if(null? m)
     #f
  (let ((len (length (car m))))
  (letrec ((h (lambda (l1 l2)
                (if(null? l1)
                   '()
                   (cons (cons (car l1) (car l2)) (h (cdr l1) (cdr l2))))))
           (b (lambda (l) (if(null? l)
                             (rep '() len)
                             (h (car l)(b (cdr l)))))))
  (b m)))))
     
;Напишете функция (main-diag m), която връща елементите от главния диагонал на m.
(define (main-diag m)
  (letrec ((h (lambda (cnt l) (if(null? l) '() (let ((p (drop cnt (car l)))) (if(null? p) '() (cons (car p) (h (inc cnt)(cdr l)))))))))
    (h 0 m)))
                     
;Напишете функция (determinant m), която пресмята детерминантата на квадратна матрица m.
;(determinant
;  '((1 0 2 -1)
;    (3 0 0 5)
;    (2 1 4 -3)
;    (1 0 5 0))) ; => 30
(define (determinant m)
  (letrec ((h (lambda (ind matrix)
                (let ((p (drop ind (car matrix))))
                  (if (equal? p '())
                      0
                      (+(* (expt -1 ind) (car p)
                      (determinant (map (lambda(x)(append (take ind x)(drop (inc ind) x))) (cdr matrix)))) (h (inc ind) matrix)))))))
  (if(equal? 2 (length m))
     (- (*(car (car m)) (cadr (cadr m))) (*(cadr (car m)) (car (cadr m))))
     (h 0 m))))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (range x y) (if (> x y) '() (cons x (range (+ x 1) y))))



(define (listToBalanceBST l)
  (if (null? l) l
  (letrec ((ll (insertion-sort l <))
           (h (lambda (x)
       (let* ((len (length l))
        (middle (quotient (+ 1 len) 2))
        (ll (zip (range 1 len) x))
        (f (lambda (p?)(lambda(x) (p? (car x) middle)))))
    (cons (map cdr (my-filter (f =) ll)) (cons (listToBalanceBST (map cdr (my-filter (f <) ll))) (listToBalanceBST (map cdr (my-filter (f >) ll)))))))))
    (h ll))))

(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define (tail-stream s) (force (cdr s)))

(define empty-stream? null?)

(define head car)

(define tail cdr) 

(define foldl (lambda (f defVal l) (if (null? l) defVal (foldl f (f defVal (head l)) (tail l)))))

(define foldl1 (lambda (f l) (foldl f (head l) (tail l))))

(define foldr (lambda (f defVal l) (if (null? l) defVal (f (head l) (foldr f defVal (tail l)))))) 

(define until (lambda (p? f val) (if (p? val) val (until p? f (f val)))))

(define delete (lambda (val l)
        (cond ((null? l) l)
              ((eq? val (head l)) (tail l))
              (else (cons (head l) (delete val (tail l)))))))

(define find (lambda (p? l)
        (cond ((null? l) l)
              ((p? (head l)) (head l))
              (else (find p? (tail l))))))

(define div quotient)

(define mod remainder)


(define filter (lambda (p? l)
               (cond ((null? l) l)
                     ((p? (head l)) (cons (head l) (filter p? (tail l))))
                     (else (filter p? (tail l))))))

(define zip (lambda (lhs rhs)
              (if (or (null? lhs) (null? rhs)) '()
                  (cons (cons (head lhs) (head rhs)) (zip (tail lhs) (tail rhs))))))

(define takeWhile (lambda (l p?)
       (if (or (null? l) (not (p? (head l)))) '() (cons (head l) (takeWhile (tail l) p?)))))

(define dropWhile (lambda (l p?)
       (if (or (null? l) (not (p? (head l)))) l (dropWhile (tail l) p?))))

(define splitWhile (lambda (l p?)
       (cons (takeWhile l p?) (list (dropWhile l p?)))))

(define groupBy (lambda (l p?)
        (if (null? l) l
            (let ((pair (splitWhile (tail l) (lambda (x) (p? x (head l))))))
              (cons (cons (head l) (head pair)) (groupBy (head (tail pair)) p?))))))

(define graphFromList (lambda (l)
     (map (lambda (ll) (cons (head (head ll)) (map (lambda (x) (tail x)) ll)))
       (groupBy (insertion-sort l (lambda (x y)
        (if (= (head x) (head y)) (< (tail x) (tail y)) (< (head x) (head y)))))
         (lambda (x y) (= (head x) (head y)))))))

(define graphToList (lambda (g)
      (foldr append '() (map (lambda (x) (map (lambda (y) (cons (head x) y)) (tail x))) g))))

(define reverseGraph (lambda (g)
       (graphFromList (map (lambda (x) (cons (tail x) (head x))) (graphToList g)))))

(define indegree (lambda (g) (map (lambda (x) (cons (head x) (length (tail x)))) g)))

(define outdegree (lambda (g) (indegree (reverseGraph g))))