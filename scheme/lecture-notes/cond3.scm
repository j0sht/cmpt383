;; cond3.scm

(load "unify.scm")

;; replace each occurence of old with new
(define deep-replace
    (lambda (old new lst)
        (cond ((equal? old lst)
                 new)
              ((list? lst)
                (map (lambda (x) (deep-replace old new x))
                     lst)
              )
              (else
                lst
              )
        )
    )
)

(define replace-matches
    (lambda (pairs lst)
        (if (null? pairs)
            lst
            (let* ((pv (car pairs))
                   (v (car pv))
                   (val1 (cdr pv))  ;; careful: val1 is a pair, so cdr is used
                   (new-lst (deep-replace v val1 lst))
                  )
                (replace-matches (cdr pairs) new-lst)
            )
        )
    )
)


(define rewrite-simple-cond1
    (lambda (lst)
        (let ((matches (unify lst
                              '(cond (?test ?val1)
                                     (else ?val2))
                       )
              )
             )
            (if (not matches)
                lst
                (replace-matches matches '(if ?test ?val1 ?val2)
                )
            )
        )
    )
)

(define test1
    '(cond ((= x 0) 'yes)
           (else 'no)
     )
)

(define test2
    '(define sc1
        (cond ((= x 0) 'yes)
              (else 'no)
        )
     )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from cond2.scm
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

(define is-simple-cond?
    (lambda (x)
        (unify x '(cond (?test ?val1)
                        (else ?val2))
        )
    )
)

(define rewrite-simple-cond-top-level
    (lambda (lst)
        (let ((matches (is-simple-cond? lst)))
            (replace-matches matches '(if ?test ?val1 ?val2))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-rewriter
    (lambda (in-pattern out-pattern)
        (lambda (x)
            (let* ((is-expr? (lambda (a) (unify a in-pattern)))
                   (rewrite-top-level (lambda (a)
                                          (replace-matches (is-expr? a) 
                                                           out-pattern)))
                  )
                (rewrite is-expr? rewrite-top-level x)
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rewrite-simple-cond
    (make-rewriter '(cond (?test ?val1)     ;; input pattern
                          (else ?val2))
                   '(if ?test ?val1 ?val2)  ;; output patten
    )
)


;; The following *doesn't work because ?args matches a list, and
;; then that entire list is put into the output pattern, e.g.:
;;
;; > (pp (rewrite-define test6))
;; (define (my-length (lst))   ;;; wrong: (lst) should be lst
;;   (cond ((null? lst) 0)
;;         (else (+ 1 (my-length (cdr lst))))))
;; 
;; But we really want in the output pattern is for ?name to be 
;; consed onto ?args.

;(define rewrite-define
;    (make-rewriter '(define ?name           ;; input pattern
;                        (lambda ?args
;                            ?body
;                        )
;                    )
;                   '(define (?name ?args)   ;; output patten
;                        ?body
;                    )
;    )
;)

(define rewrite-define-single-arg
    (make-rewriter '(define ?name          ;; input pattern
                        (lambda (?arg)
                            ?body
                        )
                    )
                   '(define (?name ?arg)   ;; output patten
                        ?body
                    )
    )
)
