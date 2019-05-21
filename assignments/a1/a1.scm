(define singleton?
  (lambda (x)
    (and (list? x)
	 (null? (cdr x)))))

(define my-make-list
  (lambda (n x)
    (if (equal? n 0)
	'()
	(cons x (my-make-list (- n 1) x)))))

(define all-same?
  (lambda (lst)
    (if (null? lst)
	#t
	(and (or (null? (cdr lst))
		 (equal? (car lst) (car (cdr lst))))
	     (all-same? (cdr lst))))))

(define my-iota
  (lambda (n)
    (let ((max n))
      (define inner-iota
	(lambda (n)
	  (if (equal? n 0)
	      '()
	      (cons (- max n) (inner-iota (- n 1))))))
      (inner-iota n))))

(define my-length
  (lambda (lst)
    (if (null? lst)
	0
	(+ 1 (my-length (cdr lst))))))

(define nth
  (lambda (lst i)
    (if (or (null? lst) (< i 0))
	(error "bad index")
	(if (equal? i 0)
	    (car lst)
	    (nth (cdr lst) (- i 1))))))

(define my-last
  (lambda (lst)
    (if (null? lst)
	(error "my-last: empty list")
	(if (null? (cdr lst))
	    (car lst)
	    (my-last (cdr lst))))))

(define middle
  (lambda (lst)
    (if (or (null? lst)
	    (null? (cdr lst))
	    (null? (cdr (cdr lst))))
	'()
	(let ((rest (cdr lst)))
	  (define inner-middle
	    (lambda (lst)
	      (if (null? (cdr lst))
		  '()
		  (cons (car lst) (inner-middle (cdr lst))))))
	  (inner-middle rest)))))

(define my-filter
  (lambda (pred? lst)
    (if (null? lst)
	'()
	(if (pred? (car lst))
	    (cons (car lst) (my-filter pred? (cdr lst)))
	    (my-filter pred? (cdr lst))))))

(define my-append
  (lambda (A B)
    (if (null? A)
	B
	(cons (car A) (my-append (cdr A) B)))))

(define append-all
  (lambda (lol)
    (if (null? lol)
	'()
	(my-append (car lol) (append-all (cdr lol))))))

;; 12. my-sort

;; 13. all-bits
(define all-bits
  (lambda (n)
    (if (<= n 0)
	'()
	(let ((max (- (expt 2 n) 1))
	      (bits n))
	  (define inner-bits
	    (lambda (n)
	      (let* ((lst (binary n))
		     (len (my-length lst)))
		(if (<= n 0)
		    (cons (my-make-list bits 0) '())
		    (cons (pad-with-zeroes (- bits len) lst)
			  (inner-bits (- n 1)))))))
	  (inner-bits max)))))

(define binary
  (lambda (n)
    (if (<= n 0)
	'()
	(cons (modulo n 2) (binary (quotient n 2))))))

(define pad-with-zeroes
  (lambda (n lst)
    (if (<= n 0)
	lst
	(my-append lst (my-make-list n 0)))))
