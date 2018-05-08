;;Daniel Urumov

;; ---helper functions--------------------------------------
(define head car)

(define tail cdr)

(define div quotient)

(define mod remainder)

(define foldl (lambda (f defVal l) (if (null? l) defVal (foldl f (f defVal (head l)) (tail l)))))

(define foldl1 (lambda (f l) (foldl f (head l) (tail l))))

(define foldr (lambda (f defVal l) (if (null? l) defVal (f (head l) (foldr f defVal (tail l)))))) 

(define until (lambda (p? f val) (if (p? val) val (until p? f (f val)))))

(define find (lambda (p? l) (cond ((null? l) #f) ((p? (head l)) (head l)) (else (find p? (tail l))))))

(define delete (lambda (p? l) (cond ((null? l) l) ((p? (head l)) (tail l)) (else (cons (head l) (delete p? (tail l)))))))

(define deleteByIndex (lambda (n l) (cond ((null? l) l) ((= 0 n) (tail l)) (else (cons (head l) (deleteByIndex (- n 1) (tail l))))))) 

(define zip (lambda (lhs rhs)
              (if (or (null? lhs) (null? rhs)) '()
                  (cons (cons (head lhs) (head rhs)) (zip (tail lhs) (tail rhs))))))

(define range (lambda (a b) (if (> a b) '() (cons a (range (+ 1 a) b)))))

(define on (lambda (f g lhs rhs) (f (g lhs) (g rhs))))

;;problem1

(define row-reduce (lambda (matrix)
        (let* ((p? (lambda (x) (not (= (head x) 0))))
               (firstRow (find p? matrix))
               (subMatrix (delete p? matrix)))
          (cons firstRow
             (map (lambda (row)
                    (map (lambda (x) (+ (head x) (tail x)))
                       (zip (map (lambda (x) (* x (head firstRow))) row)
                            (map (lambda (x) (* (- x) (head row))) firstRow))))
                  subMatrix)))))

(define problem1_test1 (row-reduce '((1 5 2) (2 3 8) (-2 0 4)))) 

;;problem2

(define cross-out (lambda (matrix)
        (let ((rows (length matrix))
              (cols (length (head matrix))))
          (map (lambda (index)
                 (let ((indexRow (div index cols)))
                   (map (lambda (row) (deleteByIndex (- index (* cols indexRow)) row))
                    (deleteByIndex indexRow matrix))))
               (range 0 (- (* rows cols) 1))))))

(define (cross-out1 matrix)
  (letrec ((row (length matrix))
           (col (length (car matrix)))
           (for (lambda (i n g f) (if (< i n) (g (f i) (for (+ 1 i) n g f)) '()))))
    (for 0 row append (lambda (i)
         (for 0 col cons
               (lambda (j) (map (lambda (r) (deleteByIndex j r)) (deleteByIndex i matrix))))))))

(define problem2_test1 (cross-out '((1 2 3 4) (5 6 7 8) (9 10 11 12))))
(define problem2_test2 (cross-out1 '((1 2 3 4) (5 6 7 8) (9 10 11 12))))


;;problem3

;;;това си е вярно
(define expr->tree (lambda (f) f))

(define tree-eval (lambda (tree x)        
    (letrec ((help        (lambda (args) (map (lambda (y) (tree-eval y x)) args)))
             (sum         (lambda (args) (foldl  + 0  (help args))))
             (product     (lambda (args) (foldl  * 1  (help args))))
             (subtraction (lambda (args) (foldl1 -    (help args))))
             (division    (lambda (args) (foldl1 /    (help args))))
             (expt1       (lambda (args) (foldl1 expt (help args)))))
         
    (cond ((number? tree)                                     tree)
          ((equal?  tree 'x)                                     x)
          ((equal? (head tree) '+)    (sum              (tail tree)))
          ((equal? (head tree) '*)    (product          (tail tree)))
          ((equal? (head tree) '-)    (subtraction      (tail tree)))
          ((equal? (head tree) '/)    (division         (tail tree)))
          ((equal? (head tree) 'expt) (expt1            (tail tree)))
          ((equal? (head tree) 'sqrt) (sqrt (head (help (tail tree)))))
          ((equal? (head tree) 'log)  (log  (head (help (tail tree)))))
          ((equal? (head tree) 'exp)  (exp  (head (help (tail tree)))))))))

(define f '(+ x 0 (* x 1 x) (sqrt (log x)) (- 1 x)))
(define test1_eval (= 2 (tree-eval f 1)))

(define tree-derive (lambda (tree)
    (letrec ((help        (lambda (args) (map tree-derive args)))
             (product     (lambda (args)
                            (let* ((left   (head args))
                                  (twoArgs (lambda ()
                                     (let ((right (cadr args)))
                                       (list '+
                                                (list '* (tree-derive left) right)
                                                (list '* left (tree-derive right))))))
                                  (manyArgs (lambda ()
                                     (let ((right (tail args)))
                                       (list '+
                                                (append (list '* (tree-derive left)) right)
                                                (list         '* left       (product right)))))))
                              (if (null? (cddr args)) (twoArgs) (manyArgs)))))
             (division    (lambda (args)
                             (let* ((l     (head args))
                                    (r     (cadr args))
                                    (left  (tree-derive l))
                                    (right (tree-derive r)))
                               (list '/ (list '- (list '* left r) (list '* l right)) (list 'expt r 2))))) 
             (sqrt1       (lambda (args)
                            (expt1 (cons (head args) (list 0.5)))))
             (log1 (lambda (args)
                     (let ((a (head args)))
                           (list '* (list '/ 1 a) (tree-derive a)))))
             (expt1 (lambda (args)
                      (let* ((l    (head args))
                             (r    (cadr args))
                             (logL (list 'log l))
                             (lr   (list '* r logL)))
                      (cond ((and (number? l) (number? r)) 0)
                            ((number? l) (list '* args logL (tree-derive r)))
                            ((number? r) (list '* r (list 'expt l (- r 1)) (tree-derive l)))
                            (else        (list '* (list 'exp lr) (tree-derive lr))))))))
         
    (cond ((number? tree)                                                 0)
          ((equal?  tree 'x)                                              1)
          ((equal? (head tree) '+)    (cons '+             (help (tail tree))))
          ((equal? (head tree) '*)    (product                   (tail tree)))
          ((equal? (head tree) '-)    (cons '-             (help (tail tree))))
          ((equal? (head tree) '/)    (division                  (tail tree)))
          ((equal? (head tree) 'expt) (expt1                     (tail tree)))
          ((equal? (head tree) 'sqrt) (sqrt1                     (tail tree)))
          ((equal? (head tree) 'log)  (log1                      (tail tree)))
          ((equal? (head tree) 'exp)  (list '* tree (tree-derive (cadr tree))))))))

(define test_derive (tree-derive f))
(define test_derive_product  (= 24 (tree-eval (tree-derive '(* x x  2 x)) 2)))
(define test_derive_log      (= 3  (tree-eval (tree-derive '(+ (log 2) (log x) (log (* x x)))) 1)))
(define test_derive_devision (= 2  (tree-eval (tree-derive '(/ (* x x x) x)) 1)))
(define test_derive_expt                      (tree-derive '(+ (expt 2 3) (expt x 3) (expt 3 x) (expt x x))))
(define test_derive_sqrt     (= 5  (tree-eval (tree-derive '(+ (expt x 2) (sqrt (expt x 2)))) 2)))