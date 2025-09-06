;; XR-Lang Kitchen Sink Example
;; Demonstrates all working features from Stage A & B

;; Basic values and types
(define x 42)
(define pi 3.14159)
(define greeting "Hello XR-Lang!")
(define enabled #t)
(define nothing nil)

;; Quote - code as data
(define code '(+ 1 2 3))

;; List operations
(define nums '(1 2 3 4 5))
(define first (car nums))
(define rest (cdr nums))
(define bigger (cons 0 nums))

;; Arithmetic
(define sum (+ 10 20 30))
(define diff (- 100 25))
(define prod (* 4 5 6))
(define quot (/ 100 4))

;; Comparison
(define less (< 3 7))
(define greater (> 10 5))
(define equal (= 5 5))

;; Lambda functions
(define square
  (lambda (n)
    (* n n)))

(define add-two
  (lambda (a b)
    (+ a b)))

;; Function application
(define sixteen (square 4))
(define total (add-two 10 20))

;; Conditional evaluation
(define result
  (if (> sum 50)
      "big"
      "small"))

;; Let bindings
(define computed
  (let ((a 100)
        (b 200))
    (+ a b)))

;; Nested let
(define nested
  (let ((x 10))
    (let ((y 20))
      (+ x y))))

;; Begin for sequential execution
(define sequential
  (begin
    (define temp 5)
    (+ temp 10)))

;; Recursive function (factorial)
(define factorial
  (lambda (n)
    (if (< n 2)
        1
        (* n (factorial (- n 1))))))

(define fact5 (factorial 5))

;; Higher-order function
(define apply-twice
  (lambda (f x)
    (f (f x))))

(define sixty-four (apply-twice square 4))

;; Map-like function
(define map-add-one
  (lambda (lst)
    (if (nil? lst)
        '()
        (cons (+ 1 (car lst))
              (map-add-one (cdr lst))))))

(define incremented (map-add-one '(1 2 3)))

;; Macro definition (when expanded)
(defmacro when (condition body)
  (list 'if condition body 'nil))

;; Using the macro
(define macro-result
  (when (> 5 3)
    (+ 100 200)))

;; Type predicates
(define type-checks
  (list
    (nil? nil)
    (bool? #t)
    (int? 42)
    (float? 3.14)
    (string? "hello")
    (symbol? 'foo)
    (list? nums)))

;; All values defined - ready for evaluation!