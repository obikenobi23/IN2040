;Oppgave 1
;a) Evaluerer til 3
;b) 5 er omgitt av parentes som et prosedyrekall: feilmelding
;c) Gal syntaks: 4 evaluerer ikke til en prosedyre
;d) Evaluerer til 22
;e) Evaluerer til 11
;f) Evaluerer til 12



;Oppgave 2
(or (= 1 2);Returnerer den første ikke-falske: "paff"
      "paff!"
      "piff!"
      (zero? (1 - 1)))

(and (= 1 2);Returnerer den første falske: #f
     "paff!"
     "piff!"
     (zero? (1 - 1)))

;Disse to er special forms fordi de kan ta imot en uendelig mengde argumenter.
;OR og AND kan returnere verdier før hele uttrykket deres er evaluert,
;som vises ved at uttrykkene beskrevet i oppgaven i boka kan kjøre uten
;å kræsje i en syntaktisk feil i (1 - 1).
;I vanlige prosedyrer må hele uttrykket evaluere, og det ville gitt feilmelding i
;dette eksemplet.

(if (positive? 42);Predikatet er sant: "poff"
    "poff!"
    (i-am-undefined))
;(i-am-undefined) vil aldri leses fordi beslutningen returnerer før
;det uttrykket evaluerer. Det sørger for at programmet ikke kræsjer i en
;udefinert prosedyre. Dette gjør if-statement til en special form.

;2b
(define (sign a)
  (if (= a 0) 0 (if (> a 0) 1 -1)))
(display "2b if\n")
;(sign 3);1
;(sign 0);0
;(sign -2);-1

(define (sign a)
  (cond ((< a 0) -1)
        ((> a 0) 1)
        (else 0)))
;(display "2b cond\n")
;(sign 0);0
;(sign 3);1
;(sign -2);-1

;2c
(define (sign a)
  (OR (AND (= a 0) 0) (AND (> a 0) 1) (AND (< a 0) -1)))
;(display "2c OR AND\n")
;(sign -3);-1
;(sign 0);0
;(sign 4);1



;Oppgave 3
(define (add1 a) (+ a 1))
(define (sub1 b) (- b 1))

;3b
(define (pluss addend1 addend2)
  (if (> addend2 0)
      (pluss (add1 addend1) (sub1 addend2))
      addend1))
;(display "pluss\n")
;(pluss 3 4);7

;3c
;Prosedyren gir opphav til en iterativ prosess.
;Dette fordi når prosedyren kaller seg selv,
;bæres all nødvendig informasjon over til neste trinn.
;I en rekursiv prosess må de tidligere trinnene huskes til slutten av evalueringen.
;Dette burde lage en rekursiv prosess:
(define (pluss addend1 addend2)
  (if (= addend2 0)
      addend1
      (pluss (add1 addend1) (sub1 addend2))))
;(pluss 3 4);7


;3d
(define (power-close-to b n)
  (define (power-iter e)
   (if (> (expt b e) n)
       e
       (power-iter (+ 1 e))))
  (power-iter 1))
;(power-close-to 2 8)
;Den internt definerte prosedyren trenger ikke bære alle
;argumentene som allerede er gitt i et øvre trinn.
;De er allerede lagret i metoden som definerer den indre prosedyren,
;og er dermed bundet i den indre metodens skop.


;3e
(define (fib n)
  (define (iter a b n)
    (if (= n 0)
        b
        (iter (+ a b) a (- n 1))))
  (iter 1 0 n))
(fib 5)
;fib-iter blir ikke forenklet som en intern prosedyre.
;Det fordi prosedyren trenger å huske alle verdiene som gis
;den i oppgaveteksten. Det er ingen verdi som kan lagres i overprosedyren.
;Da er det heller ingen ting å forenkle med blokkstruktur