(define foo 42)

;(let ((foo 5)
;      (x foo))
;  (if (= x foo)
;      'same
;      'different))

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

(let ((bar foo)
      (baz 'towel))
  (let ((bar (list bar baz))
        (foo baz))
    (list foo bar)))

;(let ((a 1) (b 2))
;  "noe")

;(lambda (a)
;   (lambda (b)
;  "noe") (1 2))

;(let* ((a 1) (b (+ a 1)))
;  "noe")