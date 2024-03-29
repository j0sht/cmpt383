(define simplify
  (lambda (expr)
    (cond
     ((null? expr) '())
     ((or (symbol? expr) (number? expr)) expr)
     (else
      (let ((expr-len (length expr)))
	(cond ((= 2 expr-len)
	       (let* ((op (car expr))
		      (n (simplify (car (cdr expr))))
		      (dec (equal? op 'dec))
		      (inc (equal? op 'inc)))
		 (if (number? n)
		     (cond (dec (- n 1))
			   (inc (+ n 1))
			   (else (error "Uknown operator" op)))
		     expr)))
	      ((= 3 expr-len)
	       (let* ((operand1 (simplify (first expr)))
		      (operator (second expr))
		      (operand2 (simplify (third expr)))
		      (plus (equal? operator '+))
		      (minus (equal? operator '-))
		      (mult (equal? operator '*))
		      (div (equal? operator '/))
		      (exp (equal? operator '**))
		      (o1-zero (equal? operand1 0))
		      (o2-zero (equal? operand2 0))
		      (o1-one (equal? operand1 1))
		      (o2-one (equal? operand2 1)))
		 (cond ((and plus o1-zero) operand2)
		       ((and plus o2-zero) operand1)
		       ((and mult (or o1-zero o2-zero)) 0)
		       ((and mult o1-one) operand2)
		       ((and mult o2-one) operand1)
		       ((and div o2-one) operand1)
		       ((and minus o2-zero) operand1)
		       ((and minus (equal? operand1 operand2)) 0)
		       ((and exp o2-zero) 1)
		       ((and exp o2-one) operand1)
		       ((and exp o1-one) 1)
		       (else (list operand1 operator operand2)))))
	      (else (error "Invalid expression"))))))))
