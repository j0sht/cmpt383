;; 1. (singleton? x)
;;
;; Returns #t if x is a list with exactly one element, #f otherwise.
(define singleton?
  (lambda (x)
    (and (list? x)
	 (null? (cdr x)))))

;; 2. (my-make-list n x)
;;
;; Returns a list containing n copies of x.
(define my-make-list
  (lambda (n x)
    (if (equal? n 0)
	'()
	(cons x (my-make-list (- n 1) x)))))

;; 3. (all-same? lst)
;;
;; Returns #t if lst is empty, or if all the elements in it are equal
;; to each other.
(define all-same?
  (lambda (lst)
    (if (null? lst)
	#t
	(and (or (null? (cdr lst))
		 (equal? (car lst) (car (cdr lst))))
	     (all-same? (cdr lst))))))

;; 4. (my-iota n)
;;
;; Returns a list containing the numbers from 0 to n-1.
(define my-iota
  (lambda (n)
    (let ((max n))
      (define inner-iota
	(lambda (n)
	  (if (equal? n 0)
	      '()
	      (cons (- max n) (inner-iota (- n 1))))))
      (inner-iota n))))

;; 5. (my-length lst)
;;
;; Returns the number of items in lst.
(define my-length
  (lambda (lst)
    (if (null? lst)
	0
	(+ 1 (my-length (cdr lst))))))

;; 6. (nth lst i)
;;
;; Returns the item at index location i in lst. Indexing is 0 based.
;; Prints error message if i < 0 or i >= length of lst.
(define nth
  (lambda (lst i)
    (if (or (null? lst) (< i 0))
	(error "bad index")
	(if (equal? i 0)
	    (car lst)
	    (nth (cdr lst) (- i 1))))))

;; 7. (my-last lst)
;;
;; Returns the last element of lst.
;; Prints error message with empty list.
(define my-last
  (lambda (lst)
    (if (null? lst)
	(error "my-last: empty list")
	(if (null? (cdr lst))
	    (car lst)
	    (my-last (cdr lst))))))

;; 8. (middle lst)
;;
;; Returns a list that is the same as lst with the first and last elements
;; removed. If lst has 2 or fewer element, returns an empty list.
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

;; 9. (my-filter pred lst)
;;
;; Returns a list containing the elements of lst that satisfy pred?.
(define my-filter
  (lambda (pred? lst)
    (if (null? lst)
	'()
	(if (pred? (car lst))
	    (cons (car lst) (my-filter pred? (cdr lst)))
	    (my-filter pred? (cdr lst))))))

;; 10. (my-append A B)
;;
;; Returns a list that has all the elements of A followed by all the
;; elements of B.
(define my-append
  (lambda (A B)
    (if (null? A)
	B
	(cons (car A) (my-append (cdr A) B)))))

;; 11. (append-all lol)
;;
;; Returns a list that has all the lists of lol appended into one list.
(define append-all
  (lambda (lol)
    (if (null? lol)
	'()
	(my-append (car lol) (append-all (cdr lol))))))

;; 12 (my-sort lst)
;;
;; Returns the numbers in lst in sorted order (ascending).
(define my-sort
  (lambda (lst)
    (define inner-sort
      (lambda (lst result)
	(if (null? lst)
	    result
	    (inner-sort (cdr lst) (insert-into (car lst) result)))))
    (inner-sort lst '())))

(define insert-into
  (lambda (n lst)
    (define inner-insert
      (lambda (n left right)
	(if (null? right)
	    (my-append left (list n))
	    (if (< n (car right))
		(my-append left (cons n right))
		(inner-insert n
			      (my-append left (list (car right)))
			      (cdr right))))))
    (inner-insert n '() lst)))

;; 13. (all-bits n)
;;
;; Returns a list of all possible 2^n bit lists. If n <= 0, returns an
;; empty list.
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
		    (list (my-make-list bits 0))
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
