;;;I don't use R5RS

;;; HASKELL
;;; roller-coaster-stream n = cycle $ [n, n-1 .. 1] ++ [2 .. n]
 
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head-stream car)
(define (tail-stream s) (force (cdr s)))
(define empty-stream? null?)
(define (take-stream s n)
  (if (or (empty-stream? s)
          (= n 0))
      '()
      (cons (head-stream s)
            (take-stream (tail-stream s) (- n 1)))))
;;;---------------------------------------      

(define range (lambda (a b)
  (if (> a b) '()
      (cons a (range (+ a 1) b)))))

(define-syntax append-stream
  (syntax-rules ()
    ((append-stream a b) (append a (delay b)))))


;пробвах да го направя по-интелигентно (виж по-долу),
;ама нещо не се изчислява,
;както аз си мисля (изчислява се строго вместо лениво),
;и нямам идея кое
(define roller-coaster-stream (lambda (n)                     
   (append-stream
                  (append
                        (reverse (range 1 n))
                        (range 2 (- n 1)))
                  (roller-coaster-stream n))))

(define test1 (equal? (take-stream (roller-coaster-stream 3) 7) '(3 2 1 2 3 2 1)))