;; Test VM Execution
;; Basic tests for VM functionality

;; Arithmetic tests
(+ 1 2 3)           ; 6
(- 10 3)            ; 7  
(* 4 5)             ; 20
(/ 20 4)            ; 5

;; Comparison tests
(< 3 5)             ; #t
(> 10 5)            ; #t
(= 5 5)             ; #t

;; List operation tests
(cons 1 '(2 3))     ; (1 2 3)
(car '(a b c))      ; a
(cdr '(a b c))      ; (b c)

;; Quote test
'(+ 1 2)            ; (+ 1 2)

;; Conditional tests
(if #t 'yes 'no)    ; yes
(if #f 'yes 'no)    ; no

;; Lambda test
((lambda (x) (* x x)) 5)  ; 25

;; Let binding test
(let ((x 10) (y 20))
  (+ x y))          ; 30

;; Define and lookup test
(define foo 42)
foo                 ; 42