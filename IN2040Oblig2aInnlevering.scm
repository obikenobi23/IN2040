;1a)

(define (p-car konscelle)
  (konscelle (lambda (argument1 argument2)
  argument1)))

(define (p-cdr konscelle)
  (konscelle (lambda (argument1 argument2)
  argument2)))

(define (p-cons x y)
  (lambda (proc) (proc x y)))


;1b)
(define foo 42)

((lambda (foo x)
      (if (= x foo)
      'same
      'different))
      5 foo)

((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
   foo 'towel)


;1c)

(define (infix-eval liste)
  ((cadr liste) (car liste) (caddr liste)))

;(infix-eval (list 1 * 20)) eventuell test


;1d)


;Quote, ', setter et quotationmark ved hvert symbol, derfor sendes ikke operatoren + inn, men heller
;symbolet +, og tallene er ikke tall, men symbol i stedet.


;Oppgave 2


;2a
  (define (decodeIter bits tree)
  (define (decode-1 bits current-branch current-string)
    (if (null? bits)
        (reverse current-string)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          ;;(display current-string))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) current-string))
              (decode-1 (cdr bits) next-branch current-string)))))
  (decode-1 bits tree '()))



;2b)
;;Kaller dekoderingsprosedyren over slik: (decodeIter sample-code sample-tree)
;;Får ut: samurais fight ninjas by night


;2c

;;Dette er en ekstraprosedyre for encode, sjekker om symbolet vi leter etter er i den grenen vi sjekker
(define (this-direction? branch symbol-looked-for)
  (cond ((element-of-set? symbol-looked-for (symbols branch)) #t)
        (else #f)))

;;Denne prosedyren er hentet fra foiler fra sesjon 5, brukes i prosedyren this-direction?.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (encode symbol-liste tree) 
  (define (encode-1 symbol-liste current-branch bitliste)
    (if (null? symbol-liste) ;;dersom symbol-listen er tom, har vi kommet til slutten og bitlisten kan returneres
        bitliste ;(display "Her er basistilfelle, avslutt rekursjon") bitliste)
    ;;(display bitliste) (newline)
    (let ((current-symbol (car symbol-liste)))
      (if (and (leaf? current-branch) (eq? current-symbol (symbol-leaf current-branch))) ;;Dersom det er en løvnode, og det symbolet vi er paa i listen er det symbolet vi har kommet til:
          (encode-1 (cdr symbol-liste) tree bitliste)
          (if (not (leaf? current-branch))
              (if (this-direction? (right-branch current-branch) current-symbol)
                  (encode-1 symbol-liste (right-branch current-branch) (append bitliste '(1)))
                  (encode-1 symbol-liste (left-branch current-branch) (append bitliste '(0))))))))) ;;else
  (encode-1 symbol-liste tree '()))

;For eventuell testing: (decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)





;2d)
(define (grow-huffman-tree frekvenspar)
  (define (grow-huffman-tree-1 leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (let* ((venstre (car leaf-set))
               (hoyre (cadr leaf-set))
                      (kombinert (adjoin-set (make-code-tree venstre hoyre)
                                             (cddr leaf-set))))
               (grow-huffman-tree-1 kombinert))))
    (grow-huffman-tree-1 (make-leaf-set frekvenspar)))


;2e)
;;Ved å tegne opp en illustrasjon av Huffman-treet som lages ved å bruke prosedyren grow-huffman
;;med den gitte listen som argument, lages et tre som koder ninja=010, fight=10, samurais=11, by=0111, night=000.
;;Med dette som utgangspunkt teller vi antall forekomster av hvert symbol og ganger med antall bits i kodingen av hvert av dem.
;;Det krever 43 bits å kode meldingen.
;;43bits delt på 17 symboler = 2,529... Det er i snitt 2,53 bits per kodeord.
;;log2(16) er 4. Det tar fire bits å kode meldingen i fastlengdekode.

;2f)
(define (huffman-leaves tree)
  (if (null? tree)
      '()
      (if (leaf? tree)
          (cons (list (symbol-leaf tree) (weight-leaf tree)) '())
          (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree))))))
