;; Scheme is like python in that you don't have to explicitly declare types

;; lambda is a macro
;; nothing gets evaluate by lambda
(define add1
    (lambda (n)
      (+ n 1) ))

(define circle-area
  (lambda (radius)
    (* 3.14 radius radius) ))

;; Alternative way to write functions
;; NOT RECOMMENDED TO WRITE THEM THIS WAY
(define (inc n)
  (+ n 1))

(define add-points
  (lambda (p1 p2)
    (list (+ (car p1) (car p2))
	  (+ (car (cdr p1)) (car (cdr p2))) )))


;; Boolean functions
;; (and b1 b2 .. bn) => Returns #t if both b1 and b2 are true; short-circuit evaluation
;; (or b1 b2 .. bn) => Returns #t if at least one expression evaluates to true (short-circuit)
;; and & or are special forms (doesn't necessarily evaluate whole expression)
(define all-diff
  (lambda (a b c)
    (and (not (equal? a b))
	 (not (equal? a c))
	 (not (equal? b c)) )))

;; cond example
;; note: = only works with numbers (equals? works with almost everything)
;; ensures something will be returned (like default)
(define sign
  (lambda (n)
    (cond ((< n 0) 'negative)
	  ((= n 0) 'zero)
	  (else 'positive) )))
