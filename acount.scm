(define acount
  (lambda (x)
    (if (equal? x 'a)
	1
	(if (list? x)
	    (if (null? x)
		0
		(let ((head (car x))
		      (tail (cdr x)))
		  (cond ((equal? head 'a) (+ 1 (acount tail)))
			((list? head) (+ (acount head) (acount tail)))
			(else (acount tail)))))
	    0))))

(define acount2
  (lambda (x)
    (cond ((equal? x 'a) 1)
	  ((and (list? x) (null? x)) 0)
	  ((list? x)
	   (let ((head (car x))
		 (tail (cdr x)))
	     (cond ((equal? head 'a) (+ 1 (acount tail)))
		   ((list? head) (+ (acount head) (acount tail)))
		   (else (acount tail)))))
	  (else 0))))



(define f
  (lambda ()
    (lambda ()
      5)))

(define my-fold
  (lambda (fn empty-val lst)
    (if (null? lst)
	empty-val
	(fn (car lst) (my-fold fn empty-val (cdr lst))))))

(define (my-length lst)
  (my-fold (lambda (x y) (+ 1 y)) 0 lst))

(define (double-up1 lst)
  (if (null? lst)
      '()
      (cons (car lst) (cons (car lst) (double-up1 (cdr lst))))))

(define (double-up2 lst)
  (my-fold (lambda (x y) (cons x (cons x y)))
	   '() lst))
