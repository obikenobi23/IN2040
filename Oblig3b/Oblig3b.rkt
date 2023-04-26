(load "Prekode-endret.rkt")
(set! the-global-environment (setup-environment))
(read-eval-print-loop)

;;1a
;;Resultater: 0, 16, 2

(define (foo cond else)
  (cond ((= cond 2) 0)
        (else (else cond))))

;;Forklaring:
;;(foo 2 square)
;;Først, når uttrykket kjøres, evalueres hele cond-uttrykket med mc-eval. Testen på special-form gir #t, siden
;;cond? gir true. Denne sjekken gir true, siden listen starter med cond. Dermed kalles eval-special-form på cond-uttrykket.
;; I eval-special-form slår testen for cond? true. Dermed konverterers cond-uttrykket til if-uttrykk, og sendes tilbake til mc-eval.
;;Dermed, når cond-variabelen så sjekkes senere i (= cond 2), vil dette deluttrykket da leses som en assignment, application?
;;vil gi true i mc-eval, og = vil sendes inn som operator, (den hentes ut av listen med prosedyren operator), og resten vil sendes
;;inn som argumneter ved hjelp av prosedyren (list-of-values). List-of-values evaluerer argumentene en om gangen.
;;Først vil den da sjekke cond-variablene i mc-eval, og der vil testen for variable? bli true, og cond vil her da få verdien til
;;det som da eventuelt ble sendt inn som argument når en kalte prosedyren foo første gang i eksempelet, her 2.
;;Når det kommer til hvorfor prosedyren velger cond-variabelen til 2, argumentet som sendes inn,
;;og ikke den globale variabelen 3: Grunnen til at prosedyren benytter seg av argumentet som sendes inn, og ikke den globale variabelen cond, satt til 3,
;;er at det er i forskjellig omgivelse. Det at cond-variabelen blir 2 skjer i en egen omgivelse, og det er denne omgivelsen som
;;sjekkes først.
;;Derfor returneres det her 2.


;;(foo 4 sqaure):
;;Prinsippet er så å si likt for else. Man kan se bort ifra at else er definert til å være en prosedyre som tar inn et argument
;;x og gjør (/ x 2) for de to første kalleksemplene. Dette er fordi når en prosedyre kalles, opprettes en lokal omgivelse, og siden denne prosedyren
;;har et parameter som heter else allerede, vil det da være det som blir else, og aldri det som er i den globale omgivelsen.
;;else i den lokale omgivelsen er her prosedyren square, det er den som har blitt sendt inn, og som det lokale parameteret else
;;i foo i dette tilfellet bindes til.


;;(cond ((= cond 2) 0)
   ;;(else (else 4)))

;;unntaket er den siste prosedyren der cond-sjekken kalles uten å være en del av en prosedyre. Som tidligere konverteres
;;cond til et if-uttrykk. Det sjekkes her om cond = 2. Nettopp her sjekker man i den globale omgivelsen, men her er cond lik 3, og ikke
;;2. Dermed slår else-uttrykket til. Else leses ikke som else her, siden cond-uttrykket er konvertert til et if-uttrykk, og
;;if-uttrykk har ikke eksplisitt else. Så dermed evalueres uttrykket (else 4). Prosedyren else fra den globale-omgivelsen
;;brukes da, og en regner så ut (* 4 4), som blir 16.



;; 2a
(1+ 5);6
(1- 4);3

;; 2b
(install-primitive! 'square (lambda (x) (* x x)))

;; 3a)
(and 1 4 #t 'b);'b
(and );#t
(or 6 'bot #f);6
(or #f 'true);'true
(or );#f


;; 3b)
; Korrekt syntaks:
(if (null? '(1 2 3))
    then #t
    elsif (= 0 1)
    then #f
    else 0)

;; 3c)
(let ((l 5)) (+ 5 l))

;; 3d)
(let x = 2 and
  y = 3 in
  (display (cons x y))
  (+ x y))