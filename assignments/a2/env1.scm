;; env1.scm
;; The environment ADT in env1 is implemented as a list of pairs
(define (make-empty-env) '())

(define extend-env
  (lambda (v val env)
    (cons (cons v val) env)))

(define apply-env
  (lambda (env v)
    (if (null? env)
	(error "apply-env: empty environment")
	(let* ((pair (car env))
	       (rest (cdr env))
	       (var (car pair))
	       (val (cdr pair))
	       (v-key (symbol-hash v))
	       (curr-key (symbol-hash var)))
	  (if (= v-key curr-key)
	      val
	      (apply-env rest v))))))

;; Test
(define test-env
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3
				      (extend-env 'b 4
						  (make-empty-env))))))
