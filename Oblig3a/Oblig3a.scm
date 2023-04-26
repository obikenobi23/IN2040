(load "prekode3a.scm")

;; Oppgave 1
(define mem
  (let ((proc-table (make-table)))
    (lambda (message proc)
      
      (define (memoize proc)
        (let ((result-table (make-table)))
          (lambda x
            (let ((previously-computed-result
                   (lookup x result-table)))
              (or previously-computed-result
                  (let ((result (apply proc x)))
                    (insert! x result result-table)
                    result))))))

      (define (unmemoize proc)
        (or (lookup proc proc-table) proc))
    
      (cond ((and (eq? message 'memoize)
                  (not (lookup proc proc-table)))
             (let ((memoized-proc
                    (memoize proc)))
               (insert! memoized-proc proc proc-table)
               memoized-proc))
            ((eq? message 'unmemoize) (unmemoize proc))
            (else proc)))))

;;Relevante kjoringseksempler:

;;(define fib (mem 'memoize fib))
;;(fib 3)
;;(fib 2)

;;(display 'unmemoize)(newline)
;;(define fib (mem 'unmemoize fib))
;;(fib 3)

;;(display 'memoize)(newline)
;;(define fib (mem 'memoize fib))
;;(fib 4)

;;(define test-proc (mem 'memoize test-proc))
;;(test-proc 41 42 43)
;;(set! test-proc (mem 'unmemoize test-proc))
;;(test-proc 41 42 43)


;;1c:
;;Prosedyren husker kun direkte loesningen for det ene kallet, her (fib 3) men ikke de andre kallene som krevdes for aa faa svaret.
;;I tabellen kan man antakelig slaa opp på hva svaret av (fib 3) er, men svaret av (fib 2), som man trengte for aa finne (fib 3) ble ikke lagt inn
;;under utregningen av (fib 3), slik som man oensket.


;;2a

;;list-to-stream:

(define (list-to-stream list-to-convert)
  (if (null? list-to-convert)
      '()
      (cons-stream (car list-to-convert) (list-to-stream (cdr list-to-convert)))))

;;stream-to-list:
;;Vi antar at en ikke trenger aa sette en grunneleggende grense for hvor mange elementer som kan konverteres,
;;men at det valgfrie argumentet er nok for aa ta hoyde for dette.
;;Dersom det forventes at en tar hoyde for at den kan vaere uendelig, ville vi satt inn at det ikke skal kunne konverteres mer enn
;;f eks 1000 elementer, og det kunne vi gjort ved aa sende inn 1000 som siste argument til prosedyren, i else-grenen til if, siste linje
;;av stream-to-list.
(define (stream-to-list list-to-convert . args)
  (cond ((stream-null? list-to-convert) '()) ;;Dersom strommen er tom, returner den tomme listen
        ((and (not (null? args)) (zero? (car args))) '()) ;;Dersom args ikke er tom, og nedtellingen, n, har naadd 0, returneres tom liste. 
         (else
          (if (and (not (null? args)) (integer? (car args))) ;;Dersom args ikke er tom, og car av args er et tall:
              (cons (stream-car list-to-convert) (stream-to-list (stream-cdr list-to-convert) (- (car args) 1))) ;;Da skal stream-car conses med resten av listen, og n telle ned
              (cons (stream-car list-to-convert) (stream-to-list (stream-cdr list-to-convert))))))) ;;Ellers, dersom det ikke finnes noe gyldig tall, kalles prosdyren uten aa bruke det.

;;Vi hadde aller foerst en versjon ved bruk av list-ref, men skrev den om til aa bruke car i stedet:
;;(define (stream-to-list list-to-convert . args)
  ;;(cond ((stream-null? list-to-convert) '())
        ;;((and (not (stream-null? args)) (list-ref args 0)(zero? (list-ref args 0))) '()) ;;prover aa sjekke hvis args finnes som tall
         ;;(else
          ;;(if (pair? args) ;;la til en if her siden fikk error paa at given (). Dersom args er par, bruk den, hvis ikke bruk den versjonen uten ekstra argumenter
              ;;(cons (stream-car list-to-convert) (stream-to-list (stream-cdr list-to-convert) (- (list-ref args 0) 1)))
              ;;(cons (stream-car list-to-convert) (stream-to-list (stream-cdr list-to-convert)))))))

;;Kjoreeksempler:
;(stream-to-list (list-to-stream '(1 2 3 4 5)))

;(stream-to-list (stream-interval 10 20))
;(show-stream nats 15)
;(stream-to-list nats 10)


;; 2b

(define (stream-take n stream)
  (if (= 0 n)
      the-empty-stream
      (cons-stream (stream-car stream) (stream-take (- n 1) (stream-cdr stream)))))


;;Kjoringseksempel:

;;(define foo (stream-take 10 nats))
;;foo
;;(show-stream foo 5)
;;(show-stream foo 20)
;;(show-stream (stream-take 15 nats) 10)

;; 2c
;; En såpass enkel tilpassing av remove-duplicates tar ikke høyde for at strømmen kan være uendelig.
;; memq kan rekursere i uendelighet, uten å finne slutten av strømmen.

;; 2d
(define (remove-duplicates stream)
  (if (stream-null? stream)
         the-empty-stream
        (cons-stream (stream-car stream)
                     (remove-duplicates
                      (stream-filter (lambda (x) (not (eq? x (stream-car stream)))) (stream-cdr stream))))))


;;Relevante kjoringseksempler:

;;(show-stream (remove-duplicates nats))
;(define ones (cons-stream 1 ones))
;(show-stream (remove-duplicates ones))