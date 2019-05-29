;; closure.scm

(define inc
  (lambda (n)
    (+ n 1)))

(define make-adder
  (lambda (n)
    (lambda (x)
      (+ x n))))

;; a closure is an object that contains a function, and also
;; an environment of variable-value pairs
(define inc3
  (let ((x 3))
    (lambda (n)
      (+ n x))))

