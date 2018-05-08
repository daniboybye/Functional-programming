;;Daniel Urumov

;; ---helper functions--------------------------------------
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

(define mreverse (lambda (l)
        (letrec ((h (lambda (buff ll)
                (if (null? ll) buff
                    (h (cons (head ll) buff) (tail ll))))))
          (h '() l))))

(define div quotient)

(define mod remainder)

(define numberToList (lambda (N)
        (if (= N 0) (list 0)
            (let ((p? (lambda (x) (= (tail x) 0)))
                  (action(lambda (x)
                    (let ((n (div (tail x) 10))
                          (l (head x))
                          (v (mod (tail x) 10)))
                    (cons (cons v l) n))))) 
            (head (until p? action (cons '() N)))))))

(define listToNumber (lambda (l) (foldl (lambda (lhs rhs) (+ (* 10 lhs) rhs)) 0 l)))

(define mmap (lambda (f l)
              (if (null? l) l
                  (cons (f (head l)) (mmap f (tail l))))))

(define filter (lambda (p? l)
               (cond ((null? l) l)
                     ((p? (head l)) (cons (head l) (filter p? (tail l))))
                     (else (filter p? (tail l))))))

(define zip (lambda (lhs rhs)
              (if (or (null? lhs) (null? rhs)) '()
                  (cons (cons (head lhs) (head rhs)) (zip (tail lhs) (tail rhs))))))

(define all (lambda (lhs rhs)
              (and lhs rhs)))

;;--problem1--------------------------------
(define argGeneric (lambda (p)
        (lambda (f l)
          (if (null? l) #f
              (let* ((lf (mmap f l))
                     (val (foldl1 p lf)))
                (tail (find (lambda(x) (= (head x) val)) (zip lf l))))))))

;;;не знам защо, ако стравнявам с eqv? или equal? няма да има съвпадение

(define argGeneric2 (lambda (p?)
        (lambda (f l)
          (if (null? l) #f
             (let ((f (lambda (lhs rhs)
                        (if (p? (f lhs) (f rhs)) lhs rhs))))
             (foldl1 f l))))))

(define argmax (argGeneric max))

(define argmin (argGeneric min))

(define testProblem1 ((lambda ()
        (let* ((argmax2 (argGeneric2 >=))
              (argmin2 (argGeneric2 <=))
              (t1 (cons (lambda (x) (* x x)) '(1 3 0 4 2.5 -4)))
              (t2 (cons (lambda (x) (* x x)) '(1 3 0 -4 2.5 4)))
              (t3 (cons length '((1 2) () (2 a 5 7) (2 4))))
              (l (list t1 t2 t3))
              (mapF (lambda (f1 f2)
                    (mmap (lambda (x)
                           (let ((f (head x)) (lst (tail x)))
                             (equal? (f1 f lst) (f2 f lst))))
                            l))))
          (foldl1 all (append (mapF argmax argmax2) (mapF argmin argmin2)))))))


;;--problem2--------------------------------
(define reduce (lambda (n)
       (let ((p? (lambda (x) (< x 10)))
             (action (lambda (x)
               (let* ((l       (numberToList x))
                     (maxDigit (foldl max 0 l)))
                 (* maxDigit (listToNumber (delete maxDigit l)))))))
       (until p? action n))))

;;---bonus--
;;разбиваме стъпката на две
;;1.в първа част намаляме числото.
   ;;случаите, в които намаляме числото най-малко са
   ;;когато най-голямата му цифра е 1 и тя в възможно най-надясно.
   ;;най-малките не едноцифрени числа, отговарящи на това условие са
   ;;10 -> 0 и 11 -> 1
;;т.е. най-малко намаляме числото 10 пъти
;;2. във втората част увеличаваме числото
;;най-много можем да го увеличавам 9 пъти
;;винаги в (1) намаляме числото повече пъти отколкото го учеличаваме в (2)
;;от (1) и (2) => на всяка стъпка намаляме числото
;;=> алгоритъмът приключва за всяко неотрицателно N

;;--problem3--------------------------------

(define findRoot (lambda (f a b eps method)
        (let* ((p? (lambda (l)(< (abs (f (head l))) eps)))
               (m (method f))
               (ll (until p? m (list a b 0))))
          (cons (head ll) (head (tail (tail ll)))))))

;; list -- lower bound, upper bound, counts iterations
(define binarySearch (lambda (f)
               (lambda (l)
                 (let* ((a (head l))
                        (b (head (tail l)))
                        (it (+ (head (tail (tail l))) 1))
                        (mid (/ (+ a b) 2)))
                   (if (> (*(f b)(f mid)) 0)
                       (list a mid it)
                       (list mid b it))))))

;; list -- xn, xn-1, counts iterations
(define sequenceMethod (lambda (f)
                (lambda (l)
                  (let* ((xn (head l))
                        (xn-1 (head (tail l)))
                        (it (+ (head (tail (tail l))) 1))
                        (xn+1 (- xn (/ (* (- xn xn-1) (f xn))(- (f xn) (f xn-1))))))
                    (list xn+1 xn it)))))

;;---------------------------------------

(define derivative (lambda (f)
             (let ((h 1e-9))
               (lambda (x)
                 (/ (- (f (+ x h)) (f x)) h)))))

;; list ---xn,nominal,count iterations
(define newtonsMethod (lambda(f)
                        (let ((p (derivative f)))
                          (lambda (l)
                            (let ((x (head l))
                                  (it (+ (head (tail (tail l))) 1)))
                              (list (- x (/ (f x) (p x))) 0 it))))))

;;---bonus2-----------------------------------------------

(define compare-methods (lambda (f a b eps)
           (let ((p1 (cons "BinarySearch" (findRoot f a b eps binarySearch)))
                 (p2 (cons "SequenceMothod" (findRoot f a b eps sequenceMethod)))
                 (p3 (cons "Newton'sMethod" (findRoot f a b eps newtonsMethod))))
             (list p1 p2 p3))))

(define test1 (compare-methods (lambda (x) (- (exp x) (* 3 x))) 0 1 1e-4))
(define test2 (compare-methods (lambda (x) (expt (- x 2) 3)) 0 5 1e-4))
(define test3 (compare-methods (lambda (x) (- (sqrt x) (* 2 (log x)))) 1 5 1e-4))
(define test4 (compare-methods (lambda (x) (- (exp (sqrt x)) (* 2 x))) 5 10 1e-4))