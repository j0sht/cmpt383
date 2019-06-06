(load "env2.scm")

(define myeval
  (lambda (expr env)
    (cond
     ((null? expr) (error "myeval: empty expression"))
     ((number? expr) expr)
     ((symbol? expr) (apply-env env expr))
     ((= (length expr) 2)
      ((apply-env ebnf-operators (first expr))
       (myeval (second expr) env)))
     ((= (length expr) 3)
      (let* ((first-expr (first expr))
	     (operator-symbol (second expr))
	     (operator (apply-env ebnf-operators operator-symbol))
	     (second-expr (third expr))
	     (first-operand (myeval first-expr env))
	     (second-operand (myeval second-expr env)))
	(if (and (equal? operator-symbol '/)
		 (equal? second-operand 0))
	    (error "Division by zero")
	    (operator first-operand second-operand)))))))

;; Helpers
(define minus-mult
  (extend-env '- - (extend-env '* * (make-empty-env))))

(define div-plus
  (extend-env '/ / (extend-env '+ + minus-mult)))

(define ebnf-operators
  (extend-env '** expt
	      (extend-env 'dec (lambda (x) (- x 1))
			  (extend-env 'inc (lambda (x) (+ x 1)) div-plus))))

;; To provide helpful error message
(define (handler x)
  (error "myeval: Unknown variable, undefined operator, or division by 0"))
(bind-default-condition-handler '() handler)

;; Testing
(define env1
  (extend-env 'x -1
	      (extend-env 'y 4
			  (extend-env 'x 1
				      (make-empty-env)))))

(define env2
  (extend-env 'm -1
	      (extend-env 'a 4
			  (make-empty-env))))

(define env3
  (extend-env 'q -1
	      (extend-env 'r 4
			  (make-empty-env))))
