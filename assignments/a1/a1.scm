(define singleton?
  (lambda (x)
    (and (list? x)
	 (equal? (cdr x) '()) )))

(define my-make-list
  (lambda (n x)
    (if (equal? n 0) '()
	(cons x (my-make-list (- n 1) x)) )))

(define all-same?
  (lambda (lst)
    (cond ((null? lst) #t)
	  (else (and (or (null? (cdr lst))
			 (equal? (car lst) (car (cdr lst))))
		     (all-same? (cdr lst)) )))))
