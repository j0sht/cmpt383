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

;; Use recursion instead of loops
(define len
  (lambda (lst)
    (cond ((null? lst) 0)
	  (else (+ 1 (len (cdr lst)))) )))


(define contains
  (lambda (x lst)
    (cond ((null? lst) #f)
	  ((equal? x (car lst)) #t)
	  (else (contains x (cdr lst))) )))

(define count-syml
  (lambda (lst)
    (cond ((null? lst) 0)
	  ((symbol? (car lst)) (+ 1 (count-syml (cdr lst))))
	  (else (count-syml (cdr lst))) )))


;; (if #t 'a 'b) ==> returns a
;; (if #f 'a 'b) ==> returns b
(define count-sym2
  (lambda (lst)
    (if (null? lst)
	0
	(+ (if (symbol? (car lst)) 1 0)
	   (count-sym2 (cdr lst)) ))))


(define count-num
  (lambda (lst)
    (cond ((null? lst) 0)
	  ((number? (car lst)) (+ 1 (count-num (cdr lst))))
	  (else (count-num (cdr lst))) )))

;; Can use ? in variable names
(define count-fn
  (lambda (pred? lst)
    (cond ((null? lst) 0)
	  ((pred? (car lst)) (+ 1 (count-fn pred? (cdr lst))))
	  (else (count-fn pred? (cdr lst))) )))

(define contains-fn
  (lambda (pred? lst)
    (cond ((null? lst) #f)
	  ((pred? (car lst)) #t)
	  (else (contains-fn pred? (cdr lst))) )))

(define contains
  (lambda (x lst)
    (contains-fn (lambda (a) (equal? a x)) lst) ))

;; (flatten '(1 2 (a b) (((3))) 4)) => (1 2 a b 3 4)
;; implementation is not very efficient, but very useful

;; May need to implement append on exam!!
;; NOTE: Not allowed to use append in assignment 1
(define flatten
  (lambda (x)
    (cond ((not (list? x)) x)
	  ((null? x) x)
	  ((list? (car x)) (append (flatten (car x))
				   (flatten (cdr x))))
	  (else (cons (car x) (flatten (cdr x)))) )))
