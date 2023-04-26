;;Oppgave 1a
(define (make-counter)
  (let ((count 0)) 
    (lambda ()
      (set! count (+ 1 count))
      count)))

;;Oppgave 2a
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

;;Oppgave 2b

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


;;Oppgave 3a
;;Pga naa som listen er endret, er 0-te element i listen fortsatt  a, og tredje er d. Men fjerde blir b siden pekeren er til
;;cdr av bar og ikke til car av bar, saa ma gaar i ring. Femte blir c, siden en gir et kall videre fra fjerde, som naa var b,
;og b sin neste blir c. 


;;Oppgave 3b
;car i bah er en peker til resten av lista. Det gjør caar bah til en peker til det
;første elementet i resten av lista (a). Når set-car! (car) vil endre caar endrer det det første elementet i den
;indre lista, som er en peker til det andre elementet i lista.

;;Oppgave 3c

;;Cycle losning 1:
(define (cycle? liste)
  (define (end-element liste)
    (if (pair? (cdr liste)) (end-element (cdr liste))
        liste))
  (if (eq? (end-element liste) (car liste)) #t
      #f))

;;Cycle losning 2:
(define (cycle? liste)
  ((not (list? liste))))

;;Oppgave 3d
;I Scheme er en liste en datastruktur som ender med den tomme listen. Siden en sirkulær liste er en datastruktur som ender med en
;referanse til seg selv, er ikke denne datatrukturen en liste for Schemes evaluator. Vi kaller denne strukturen en liste fordi den kan brukes som en liste,
;og den dannes fra en ekte liste. "Under panseret" er den derimot ikke en liste