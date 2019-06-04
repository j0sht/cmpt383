;; Scheme lets you do things like this:
(define min-and-max (list min max))
((car min-and-max) 41 2 3) ;; => 2
;; The sub-expression (list min max) evaluates to (list <min-fn> <max-fn>)
;; i.e. the variables min and max are replaced by the functions they're
;; bound to.

;; Using a ' does not give the same result
((car '(min max)) 41 2 3)) ;; => object min is not applicable

;; Difference here is that the min inside the quoted list is not evaluated
;; so the result of (car '(min max)) is just the symbol min.
;; Takeaway => items in a quoted list are not evaluated

;; In contrast, when (list min max) is called, both min and max are evaluated
;; and replaced with their corresponding functions
;; Takeaway => list function evaluates its arguments before creating list
