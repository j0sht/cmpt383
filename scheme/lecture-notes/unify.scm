;; unify.scm

;; The following is based on the LISP unification code from the 5th
;; edition of Luger's Artificial Intelligence textbook.
;; 
;; Variables ?x are used instead of (var x), and when unification fails, 
;; #f is returned instead of 'failed.
;;
;; Unifies the two given patterns, returning an association list of
;; pairs indicating variable bindings. If the patterns match with no
;; bindings (e.g. they are the same pattern), then '() is returned. If
;; the patterns don't match, #f is returned.

;; when true, occurs checking is done; when false, it is not done
;; (and thus more efficient, but runs the risk of an infinite loop)
(define occurs-check-enabled #t)

(define (unify pat1 pat2)
  (unify-aux pat1 pat2 '()))

(define (unify-aux pat1 pat2 sub)
  (cond ((equal? sub #f) #f)
    ((var? pat1) (match-var pat1 pat2 sub))
    ((var? pat2) (match-var pat2 pat1 sub))
    ((constant? pat1)
     (if (equal? pat1 pat2)
         sub
         #f))
    ((constant? pat2) #f)
    (else (unify-aux (cdr pat1) (cdr pat2)
             (unify-aux (car pat1) (car pat2) sub)))))

;; bug fix: Original code looped forever on (unify '(?x ?y a) '(?y ?x ?x)).
;; Code below fixes this by checking if pat is a variable, and, if it is,
;; unifying on pat's value.
;;
;; In PAIP, Norvig also points out that this causes problems:
;;
;; > (unify '(f (?x ?y a) (?y ?x ?x)) '(f ?z ?z))
;; ((?y . a) (?x . ?y) (?z ?x ?y a))
(define (match-var var pat sub)
  (if (equal? var pat) 
      sub
      (let ((binding (get-binding var sub)))
    (cond ((not (null? binding)) 
           (unify-aux (get-binding-value binding) pat sub))
          ((and (var? pat) (not (null? (get-binding pat sub))))
           (unify-aux var (get-binding-value (get-binding pat sub)) sub))
          ((and occurs-check-enabled (occurs? var pat)) #f)
          (else (add-substitution var pat sub))))))

(define (occurs? var pat)
  (cond ((equal? var pat) #t)
    ((or (var? pat) (constant? pat)) #f)
    (else (or (occurs? var (car pat))
          (occurs? var (cdr pat))))))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (constant? item)
  (or (null? item) (atom? item)))
    
;; vars are atoms beginning with ?
(define (var? a)
  (if (symbol? a)
      (let ((s (symbol->string a)))
         (and (< 1 (string-length s))
              (eq? #\? (string-ref s 0))))
      #f))

;; Adds (key . val) to the given association, returning the new alist.
(define (acons key val alist)
  (cons (cons key val) alist))

;; Common LISP's assoc returns nil when there's no match,
;; so that is simulated here.
;; http://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm
(define (get-binding var sub)
  (let ((result (assoc var sub)))
    (if result
    result
    '())))

(define (get-binding-value binding)
  (cdr binding))

(define (add-substitution var pat sub)
  (acons var pat sub))


;; From lecture:
(define make-recognizer
  (lambda (e1)
    (lambda (e2)
      (if (unify e1 e2)
	  #t
	  #f))))

(define is-and? (make-recognizer '(?x and ?y)))
(define is-or? (make-recognizer '(?x or ?y)))
(define is-not? (make-recognizer '(not ?x)))
