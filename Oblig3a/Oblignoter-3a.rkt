(load "Prekode.rkt")

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

(define fib (mem 'memoize fib))
(fib 3)
(fib 2)

(display 'unmemoize)(newline)
(define fib (mem 'unmemoize fib));; Glemmer ikke tidligere utregninger?
(fib 3)

(display 'memoize)(newline)
(define fib (mem 'memoize fib))
(fib 4)

(define test-proc (mem 'memoize test-proc))
(test-proc 41 42 43)
(set! test-proc (mem 'unmemoize test-proc))
(test-proc 41 42 43)

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

(show-stream (remove-duplicates nats))
;(define ones (cons-stream 1 ones))
;(show-stream (remove-duplicates ones))