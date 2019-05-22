;; 12. my-sort
;; NOTE: Code written is my own work, however I did use code found here:
;; http://gauss.ececs.uc.edu/Courses/C511/html/Scheme/merge.ss.html
;; as reference to understand merge sort in Scheme
(define my-sort
  (lambda (lst)
    (cond ((null? lst) '())
	  ((singleton? lst) lst)
	  (else (let* ((lsts (split lst))
		       (lst1 (car lsts))
		       (lst2 (car (cdr lsts))))
		  (merge (my-sort lst1) (my-sort lst2)))))))

(define split
  (lambda (lst)
    (let* ((len (my-length lst))
	   (half-n (quotient len 2)))
      (define first-half
	(lambda (n lst)
	  (if (equal? n half-n)
	      '()
	      (cons (car lst) (first-half (- n 1) (cdr lst))))))
      (define second-half
      	(lambda (n lst)
      	  (if (equal? n half-n)
      	      lst
      	      (second-half (- n 1) (cdr lst)))))
      (cons (first-half len lst) (cons (second-half len lst) '())))))

(define merge
  (lambda (lst1 lst2)
    (cond ((and (null? lst1) (null? lst2)) '())
	  ((null? lst1) lst2)
	  ((null? lst2) lst1)
	  (else (let ((a (car lst1))
		      (b (car lst2))
		      (a-rest (cdr lst1))
		      (b-rest (cdr lst2)))
		  (if (< a b)
		      (cons a (merge a-rest lst2))
		      (cons b (merge lst1 b-rest))))))))
