;; returns true iff lst is a list of length n
(define nlist?
  (lambda (n lst)
    (and (list? lst)
	 (= n (length lst)))))

;; returns true iff the first element of lst equals x
(define starts-with
  (lambda (x lst)
    (equal? x (car lst))))

(define is-simple-cond?
  (lambda (lst)
    (and (nlist? 3 lst)
	 (starts-with 'cond lst)
	 (nlist? 2 (second lst))
	 (nlist? 2 (third lst))
	 (member (car (third lst)) '(else #t)))))

(define rewrite-simple-cond-top-level
  (lambda (lst)
    (list 'if
	  (car (second lst))
	  (second (second lst))
	  (second (third lst)))))

(define rewrite
  (lambda (is-expr? rewrite-top-level lst)
    (cond ((is-expr? lst)
	   (rewrite is-expr? rewrite-top-level (rewrite-top-level lst)))
	  ((list? lst)
	   (map (lambda (x) (rewrite is-expr? rewrite-top-level x))))
	  (else lst))))
