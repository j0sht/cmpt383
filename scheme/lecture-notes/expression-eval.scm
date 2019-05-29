(define is-true? (lambda (e) (equal? e 't)))
(define is-false? (lambda (e) (equal? e 'f)))

(define is-literal?
    (lambda (e)
        (or (is-false? e)
            (is-true? e))))

;; returns true iff lst is a list of length n
(define nlist?
    (lambda (n lst)
        (and (list? lst)
             (= n (length lst)))))

;; (not expr)
(define is-not?
    (lambda (e)
        (and (nlist? 2 e)
             (equal? 'not (car e)))))

;; (p op q)
(define is-bin-op?
    (lambda (e op)
        (and (nlist? 3 e)
             (equal? op (second e)))))

(define is-and? (lambda (e) (is-bin-op? e 'and)))    ;; (p and q)
(define is-or? (lambda (e) (is-bin-op? e 'or)))      ;; (p or q)

;; Returns true iff e is a valid propositional expression.
(define is-expr?
    (lambda (e)
        (cond
            ((is-literal? e)
                #t)
            ((is-not? e)
                (is-expr? (second e)))
            ((is-and? e)
                (and (is-expr? (first e))
                     (is-expr? (third e))))
            ((is-or? e)
                (and (is-expr? (first e))
                     (is-expr? (third e))))
            (else #f))))

;; Returns true iff e is a valid propositional expression.
(define is-expr?
    (lambda (e)
        (cond
            ((is-literal? e)
                #t)
            ((is-not? e)
                (is-expr? (second e)))
            ((is-and? e)
                (and (is-expr? (first e))
                     (is-expr? (third e))))
            ((is-or? e)
                (and (is-expr? (first e))
                     (is-expr? (third e))))
            (else
                #f))))

(define eval-prop
 (lambda (e)
   (if (eval-prop-bool e) 't 'f)))

(define simplify
  (lambda (e)
    (cond ((is-literal? e) e)
	  ((and (is-not? e) (is-not? (second e))) (simplify (second (second e))))
	  ((is-not? e) (list 'not (simplify (second e))))
	  ((is-and? e) (list (simplify (car e)) 'and (simplify (third e))))
	  ((is-or? e) (list (simplify (car e)) 'or (simplify (third e))))
	  (else (error "invalid expression")))))
	  
(define is-prop-literal?
  (lambda (x)
    (symbol? x)))

(define make-nand
  (lambda (p q)
    (list p 'nand q)))

(define nand-rewrite
  (lambda (e)
    (cond ((is-prop-literal? e) e)
	  ((is-not? e) (let ((np (nand-rewrite (second e))))
			 (make-nand np np)))
	  ((is-and? e)
	   (let* ((p (nand-rewrite (first e)))
		  (q (nand-rewrite (third e)))
		  (pnq (make-nand p q)))
	     (make-nand pnq pnq)))
	  ((is-or? e)
	   (let* ((p (nand-rewrite (first e)))
		  (q (nand-rewrite (third e)))
		  (pnp (make-nand p p))
		  (qnq (make-nand q q)))
	     (make-nand pnp qnq))))))
