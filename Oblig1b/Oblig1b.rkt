; Oppgave 1
;; Tegning skal legges til

;f) (car (cdr '(0 42 #t bar)))
;g) (car (cdr (car '((0 42) (#t bar)))))
;h) (car (car (cdr '((0) (42 #t) (bar)))))
;i) (cons (cons 0 42) (cons #t 'bar))
; (list (list 0 42) (list #t 'bar))

; Hva skjer på i)? Spørre Jørgen?

; Oppgave 2
;2a)
(define (take n i)
    (cond ((< (length i) n)
             i)
          ((null? i)
                 '())
          ((= 1 n)
             (cons (car i) '()))
          (else
           (cons (car i) (take (- n 1) (cdr i))))))
(take 3 '(a b c d e f));(a b c)
(take 1 '(a b c d e f));(a)
(take 4 '(a b));(a b)
(take 4 '());()


;2b)
(define (take n i)
    (define (cons-take m i)
      (cond ((null? i)
             '())
            ((< (length i) n)
              i)
            ((= (- n 1) m)
             (cons (car i) '()))
            (else
             (cons (car i) (cons-take (+ m 1) (cdr i))))))
    (cons-take 0 i))
(take 3 '(a b c d e f));(a b c)
(take 1 '(a b c d e f));(a)
(take 4 '(a b));(a b)
(take 4 '());()

;2c)
(define (take-while pred items)
  (if (pred (car items))
      (cons (car items) (take-while pred (cdr items)))
      '()))
;(take-while zero? '(0 0 2 3 4 0 6 7 8 9))

;2d)
(define (map2 proc dimension1 dimension2)
      (cond
        ((OR (null? dimension1) (null? dimension2))
         '())
        (else
         (cons (proc (car dimension1) (car dimension2)) (map2 proc (cdr dimension1) (cdr dimension2))))))


(map2 + '(1 2 3) '(1 2 3 4));(2 4 6)
(map2 + '(1 2 3 4) '(3 4 5));(4 6 8)

;2e)
(map2 (lambda(x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5));(2 3 4)