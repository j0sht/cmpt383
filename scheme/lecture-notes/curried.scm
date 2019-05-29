;; Gottlob Frege (1848 - 1925)
;; - Father of analytical philosophy
;; - Logician, mathematician
;; - 1st-order predicate
;; - The function idea

;; Moses Schonfinkel (1889 - 1942)
;; - Mathematician, logician
;; - Combinatory logic
;; - "Used the idea extensively"

;; Haskell Curry (1900 - 1982)
;; - Made combinatory logic "popular"
;; - Credits Schonfinkel for the "idea"
;; - "Idea" ==> Currying
;; - ex) f(x, y) = x + y
;;       f(2, y) = 2 + y <==> f(y) = 2 + y

;; An uncurried definition:
(define add_a
  (lambda (x y)
    (+ x y)))

;; Curried:
(define add_b
  (lambda (x)
    (lambda (y)
      (+ x y))))

;; One of the biggest downfalls of scheme is that it doesn't support currying
;; very nicely. Ex) add_b must be called like ((add_b 3) 4), not (add_b 3 4)

(define curry_2
  (lambda (f)
    (lambda (x)
      (lambda (y)
	(f x y)))))

(define inc ((curry_2 +) 1))

(define f ((curry_2 filter) odd?))

;; Homeword
;; Write a function that takes a curried function and returns an un-curried version
