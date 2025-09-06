;; Stage A: Homoiconicity Demo
;; Code as data principle

;; Quote prevents evaluation - code becomes data
(define code '(+ 1 2 3))

;; Eval turns data back into code
(define result (eval code))  ; 6

;; Build code dynamically
(define make-adder
  (lambda (n)
    (list 'lambda '(x) (list '+ 'x n))))

;; Create an add-5 function
(define add5-code (make-adder 5))
(define add5 (eval add5-code))
(define test (add5 10))  ; 15

;; Manipulate code as lists
(define expr '(* 2 3))
(define first-op (car expr))     ; *
(define args (cdr expr))          ; (2 3)
(define new-expr (cons '+ args)) ; (+ 2 3)
(define new-result (eval new-expr)) ; 5