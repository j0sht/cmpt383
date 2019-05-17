(define singleton?
  (lambda (x)
    (and (list? x)
	 (equal? (cdr x) '()) )))
