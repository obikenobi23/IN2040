;;1a
(define (make-counter)
  (define increment 0)
  (define (counter)
    (set! increment (+ increment 1))
    increment)
  counter)

;;2a
(define (make-stack start)
  (let ((liste start))
    (define (push! args)
      (if (null? args)
          '()
          (begin (set! liste (cons (car args) liste))
                 (push! (cdr args)))))
    (define (pop!)
      (if (null? liste) '()
          (begin (set! liste (cdr liste))
                 liste)))
    (define (stack)
      liste)
    (lambda (msg . elements)
      (cond ((eq? msg 'push!) (push! elements))
            ((eq? msg 'pop!) (pop!))
            ((eq? msg 'stack) (stack))
            (else (display "Could not recognize input -- make-stack")))))
  )

;;2b
(define (pop! obj)
  (obj 'pop!))

(define (stack obj)
  (obj 'stack))

(define (push! . args)
  (let ((obj (car args)))
    (let ((args (cdr args)))
      (define (push-recurse! stack-bot)
        (if (null? stack-bot)
            '()
            (begin (obj 'push! (car stack-bot))
                   (push-recurse! (cdr stack-bot)))))
      (push-recurse! args))))

;;3b
;car i bah er en peker til resten av lista. Det gjør caar bah til en peker til det
;første elementet i resten av lista (a). Når set-car! (car) vil endre caar endrer det det første elementet i den
;indre lista, som er en peker til det andre elementet i lista.

;;3c
(define (cycle? liste)
  (define (end-element liste)
    (if (pair? (cdr liste)) (end-element (cdr liste))
        liste))
  (if (eq? (end-element liste) (car liste)) #t
      #f))

;;3d
;I Scheme er en liste en datastruktur som ender med den tomme listen. Siden en sirkulær liste er en datastruktur som ender med en
;referanse til seg selv, er ikke denne datatrukturen en liste for Schemes evaluator. Vi kaller denne strukturen en liste fordi den kan brukes som en liste,
;og den dannes fra en ekte liste. "Under panseret" er den derimot ikke en liste