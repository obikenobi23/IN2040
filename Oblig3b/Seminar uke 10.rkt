;; Ikke prøv å forstå all prekoden!
;; Overskriv prosedyrene fra prekoden

(load "Prekode.rkt")

;; Last inn REPL
;(read-eval-print-loop)

;; Evaluer (+ 1 2)

(set! the-global-environment (setup-environment))

;; Se på den globale omgivelsen
;the-global-environment
(read-eval-print-loop)