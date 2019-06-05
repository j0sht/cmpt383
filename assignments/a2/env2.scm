;; env2.scm
;; The environment ADT in env2 is implemented as a Binary Search Tree
(define (make-empty-env) '())

(define extend-env
  (lambda (v val env)
    (insert (cons v val) env)))

;; BST Functions
;; Citation: Code below is modified version of code found here:
;; http://www.sfu.ca/~tjd/383summer2019/scheme-intro-cont.html
;; Under "Example: Binary Search Tree"
(define (value t) (first t))
(define (left t) (second t))
(define (right t) (third t))
(define get-key
  (lambda (pair)
    (symbol-hash (car pair))))

(define apply-env
  (lambda (env v)
    (if (null? env)
	(error "apply-env empty environment")
	(let* ((P (value env))
	       (L (left env))
	       (R (right env))
	       (p-key (get-key P))
	       (v-key (symbol-hash v)))
	  (cond
	   ((= v-key p-key) (cdr P))
	   ((< v-key p-key) (apply-env L v))
	   (else (apply-env R v)))))))

(define insert
  (lambda (x t)
    (if (null? t)
	(list x '() '())
	(let* ((P (value t))
	       (L (left t))
	       (R (right t))
	       (p-key (get-key P))
	       (x-key (get-key x)))
	  (cond
	   ((= x-key p-key) (cons x (cdr t)))
	   ((< x-key p-key) (list P (insert x L) R))
	   (else (list P L (insert x R))))))))

;; Test
(define test-env
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3
				      (extend-env 'b 4
						  (make-empty-env))))))
