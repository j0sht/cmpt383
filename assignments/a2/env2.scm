;; env2.scm
;; The environment ADT in env2 is implemented as a Binary Search Tree
(define (make-empty-env) '())

(define extend-env
  (lambda (v val env)
    (insert (cons v val) env)))

(define apply-env
  (lambda (t v)
    (if (null? t)
	(error "apply-env empty environment")
	(let* ((P (value t))
	       (L (left t))
	       (R (right t))
	       (p-key (get-key P))
	       (v-key (symbol-hash v)))
	  (cond
	   ((= v-key p-key) (cdr P))
	   ((< v-key p-key) (apply-env L v))
	   (else (apply-env R v)))))))

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

(define insert
  (lambda (x t)
    (if (null? t)
	(list x '() '())
	(let* ((V (value t))
	       (L (left t))
	       (R (right t))
	       (v-key (get-key V))
	       (x-key (get-key x)))
	  (cond
	   ((= x-key v-key) (cons x (cdr t)))
	   ((< x-key v-key) (list V (insert x L) R))
	   (else (list V L (insert x R))))))))

;; Test
(define test-env
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3
				      (extend-env 'b 4
						  (make-empty-env))))))
