;; XR-Lang Simple Demo
;; Basic working examples

;; Define values
(define x 10)
(define y 20)
(define message "Hello World")

;; Simple arithmetic
(+ x y)         ; 30
(* x 3)         ; 30
(- y x)         ; 10
(/ y 2)         ; 10

;; Conditionals
(if (> x 5)
    "x is big"
    "x is small")

;; Functions
(define double
  (lambda (n)
    (* n 2)))

(double 21)     ; 42

;; Lists
(define lst '(1 2 3))
(car lst)       ; 1
(cdr lst)       ; (2 3)
(cons 0 lst)    ; (0 1 2 3)