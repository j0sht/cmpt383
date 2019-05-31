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

;; curried versions of standard functions
(define c_+ (curry_2 +))
(define c_cons (curry_2 cons))
(define c_filter (curry_2 filter))
(define inc ((curry_2 +) 1))
(define odds ((curry_2 filter) odd?))
(define add_cherry (c_cons 'cherry))

;; Homework
;; Write a function that takes a curried function and returns
;; an un-curried version
(define uncurry_2
  (lambda (f) ;; f is curried, 2 inputs
    (lambda (x y)
      ((f x) y))))

(define plus (uncurry_2 c_+))
