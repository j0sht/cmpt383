;; compose.scm

(define f
  (lambda (x)
    (* x x)))

(define g
  (lambda (x)
    (+ (* 2 x) 1)))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; h = f(g(x))
(define h (compose f g))

(define my-second
  (compose car cdr))

(define twice
  (lambda (f)
    (compose f f)))

(define garnish
  (twice (lambda (x) (cons 'cheese x))))

(define compose-n
  (lambda (f n)
    (if (= n 1)
	f
	(compose f (compose-n f (- n 1))))))

(define triple-cherry
  (compose-n (lambda (lst) (cons 'cherry lst)) 3))
