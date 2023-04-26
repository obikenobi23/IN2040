;;;;
;;;; Prekode til innlevering 2a i IN2040 (H22): Prosedyrer for å jobbe med
;;;; Huffman-trær, fra SICP, Seksjon 2.3.4.
;;;;

;;; Merk at koden under gjør bruk av diverse innebygde kortformer for
;;; kjeder av car og cdr. F.eks er (cadr x) det samme som (car (cdr x)), 
;;; og (caadr x) tilsvarer (car (car (cdr x))), osv. 



;;;
;;; Abstraksjonsbarriere for trær:
;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;;;
;;; Dekoding:
;;;

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))


;;;
;;; Sortering av node-lister:
;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;;; Eksempel på kodebok:
(define sample-tree
  (make-code-tree
   (make-code-tree
    (make-leaf 'fight 6)
    (make-leaf 'ninjas 5))
   (make-code-tree
    (make-leaf 'samurais 4)
    (make-code-tree
     (make-leaf 'night 2)
     (make-leaf 'by 1)))))

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

(define (this-direction? branch symbol-looked-for)
  (cond ((element-of-set? symbol-looked-for (symbols branch)) #t)
        (else #f)))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode symbol-liste tree)
  (define (encode-1 symbol-liste current-branch bitliste)
    ;(display bitliste)
    (if (null? symbol-liste)
        bitliste
        (let ((current-symbol (car symbol-liste)))
          (if (and (leaf? current-branch) (equal? current-symbol (symbol-leaf current-branch)))
              (encode-1 (cdr symbol-liste) tree bitliste)
              (if (not (leaf? current-branch))
                  (if (this-direction? (right-branch current-branch) current-symbol)
                      (encode-1 symbol-liste (right-branch current-branch) (append bitliste '(1)))
                      (encode-1 symbol-liste (left-branch current-branch) (append bitliste '(0)))))))))
  (encode-1 symbol-liste tree '()))

(decode (encode '(ninjas fight samurais) sample-tree) sample-tree)
;(display "ninjas")(newline)
(encode '(ninjas) sample-tree)
;(display "fight")(newline)
(encode '(fight) sample-tree)
;(display "samurais")(newline)
(encode '(samurais) sample-tree)
;(display "by")(newline)
(encode '(by) sample-tree)
;(display "night")(newline)
(encode '(night) sample-tree)


(define (huffman-leaves tree)
  (if (null? tree)
      '()
      (if (leaf? tree)
          (cons (list (symbol-leaf tree) (weight-leaf tree)) '())
          (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree))))));;Ikke helt riktig

;;; Eksempelkode: 
(define sample-code '(1 0 0 0 0 1 1 1 1 1 1 0))
;(decode sample-code sample-tree)

(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
;(decode (encode '(a b c) codebook) codebook)

;(huffman-leaves sample-tree)

(define opg-e '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2) (defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))
(define grown (grow-huffman-tree opg-e))
;grown
;(encode '(ninjas fight ninjas) grown)



;;Ved å tegne opp en illustrasjon av Huffman-treet som lages ved å bruke prosedyren grow-huffman
;;med den gitte listen som argument, lages et tre som koder ninja=010, fight=10, samurais=11, by=0111, night=000.
;;Med dette som utgangspunkt teller vi antall forekomster av hvert symbol og ganger med antall bits i kodingen av hvert av dem.
;;Det krever 43 bits å kode meldingen.
;;43bits delt på 17 symboler = 2,529... Det er i snitt 2,53 bits per kodeord.
;;log2(16) er 4. Det tar fire bits å kode meldingen i fastlengdekode.