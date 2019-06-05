;; env1.scm
;; The environment ADT in env1 is implemented as a list of pairs
(define (make-empty-env) '())

(define extend-env
  (lambda (v val env)
    (cons (cons v val) env)))

(define apply-env
  (lambda (env v)
    (let ((result (filter (lambda (x) (equal? (car x) v)) env)))
      (if (null? result)
	  (error "apply-env: empty environment")
	  (cdr (car result))))))

;; Test
(define test-env
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3
				      (extend-env 'b 4
						  (make-empty-env))))))
