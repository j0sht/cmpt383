;; cond2.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns true iff lst is a list of length n
(define nlist?
    (lambda (n lst)
        (and (list? lst) 
             (= n (length lst))
        )
    )
)

;; returns true iff the first element of lst equals x
(define starts-with
    (lambda (x lst)
        (equal? x (car lst))
    )
)


(define test1 
    '(define sc1
        (cond ((= x 0) 'yes)
              (else 'no)
        )
     )
)

(define test2
  '(define rewrite-simple-cond-aux
    (lambda (lst)
      (cond ((is-simple-cond? lst)
            (let ((test1 (car (second lst)))
                (val1 (second (second lst)))
                (val2 (second (third lst)))
                 )
               (list 'if (rewrite-simple-cond test1) 
                         (rewrite-simple-cond val1)
                         (rewrite-simple-cond val2)
               )
            ))
          (else ;; not a simple cond, nothing to do 
            lst
          )
      ) ;; cond
    )
  )
)

(define test3
    '(define sc1
        (if (= x 0) 'yes 'no)
     )
)

(define test4
    '(+ 1 x)
)

(define test5
    '(+ x 1)
)

(define test6
    '(define my-length
        (lambda (lst)
            (cond
                ((null? lst)
                    0)
                (else
                    (+ 1 (my-length (cdr lst))))
            )
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; generalized re-writer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;; The rewrite requires two functions to work:
;;
;;   (is-expr? x) 
;;       Tests if x is a valid expr for rewrite-top-level.
;;
;;   (rewrite-top-level x) 
;;      Returns a re-written expression that evaluates to the same thing as x. 
;;      It can assume that (is-expr? x) is true. Also, it only has to do a
;;      a top-level rewrite of x, i.e. it does *not* need to recursively call
;;      rewrite sub-expressions of x. That is done automatically by rewrite.
;;
;; Here;s an example call:
;;
;;   (pp (rewrite is-def-lambda? def-lambda-rewrite test6))
;;
;; pp is a pretty-printer, which prints the output in a more human-friendly
;; format.
(define rewrite
    (lambda (is-expr? rewrite-top-level lst)
        (cond ((is-expr? lst)
                (rewrite is-expr? rewrite-top-level (rewrite-top-level lst)))
              ((list? lst)
                (map (lambda (x) (rewrite is-expr? rewrite-top-level x))
                     lst))
              (else
                lst)
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewrite increment-by-1 expressions to +1 expressions.
;;
;;  (+ 1 x) --> (+1 x)
;;  (+ x 1) --> (+1 x)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define is-inc?
    (lambda (x)
        (and (nlist? 3 x)
             (starts-with '+ x)
             (or (equal? 1 (second x))
                 (equal? 1 (third x))
             )
        )
    )
)

;; Pre-condition:
;;    (is-inc? lst)
(define inc-rewrite-top-level
    (lambda (lst)
        (let ((left (second lst))
              (right (third lst))
             )
             (list '1+
                   (if (equal? 1 left) right left))
        )
    )
)

(define rewrite-inc 
    (lambda (x) (rewrite is-inc? inc-rewrite-top-level x))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rewrite a define whose body is a lambda to an equivalent short define.
;;
;; Converts this:
;;
;;    (define func
;;       (lambda (a1 a2 ... an)
;;          body
;;       )
;;    )
;;
;; To this:
;;
;;    (define (func a1 a2 ... an)
;;        body
;;    )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-lambda?
    (lambda (lst)
        (and (nlist? 3 lst)
             (starts-with 'lambda lst)
             (list? (second lst))
        )
    )
)

(define is-def-lambda?
    (lambda (lst)
        (and (nlist? 3 lst)
             (starts-with 'define lst)
             (is-lambda? (third lst))
        )
    )
)

(define def-lambda-rewrite-top-level
    (lambda (lst)
        (let ((name (second lst))
              (args (second (third lst)))
              (body (third (third lst)))
             )
            (list 'define
                  (cons name args)
                  body
            )
        )
    )
)

(define rewrite-def-lambda 
    (lambda (x) (rewrite is-def-lambda? def-lambda-rewrite-top-level x))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rewrite a "simple cond" as an equivalent if expression
;;
;; Converts this:
;;
;;    (cond (test val1)
;;          (else val2)  ;; else could instead be #t
;;    )
;;
;; To this:
;;
;;    (if test
;;        val1
;;        val2
;;    )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-simple-cond?
    (lambda (lst)
        (and (nlist? 3 lst)
             (starts-with 'cond lst)
             (nlist? 2 (second lst))
             (nlist? 2 (third lst))
             (member (car (third lst)) '(else #t))
        )
    )
)

;; Pre-condition:
;;   (is-simple-cond? lst)
(define rewrite-simple-cond-top-level
  (lambda (lst)
    (list 'if (car (second lst))
              (second (second lst))
              (second (third lst)))
  )
)

(define rewrite-simple-cond
    (lambda (x) (rewrite is-simple-cond? rewrite-simple-cond-top-level x))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rewrite an if expression to an equivalent cond expression.
;;
;; Converts this:
;;    (if test
;;        val1
;;        val2
;;    )
;;
;; To this:
;;
;;    (cond (test val1)
;;          (else val2)  ;; else could instead be #t
;;    )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (if a b c)
(define is-if?
  (lambda (lst)
    (and (nlist? 4 lst)
         (starts-with 'if lst)
    )
  )
)

;; Pre-condition:
;;   (is-if? lst)
(define rewrite-if-top-level
    (lambda (lst)
        (let ((test (second lst))
              (val1 (third lst))
              (val2 (fourth lst))
             )
           (list 'cond
               (list test val1)
               (list 'else val2)
           )
        )
    )
)

(define rewrite-if
    (lambda (x) (rewrite is-if? rewrite-if-top-level x))
)

;;
;; Some other things to try:
;;
;; - A test-value pair of the form (#f val) in a cond-expression can
;;   can be removed since val will never be chosen.
;;
;; - Constant folding: an expression like (+ 1 (- 3 2)) could be evaluated
;;   an replaces with 2. Similarly, boolean expressions could also be 
;;   evaluated in some cases, e.g. (or #f #t) is the just #t, and
;;   (and (or #f #f) x y z) is #f.
;;
