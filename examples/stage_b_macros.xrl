;; Stage B: Macro Demo
;; Shows basic macro expansion

;; Simple macro that expands inline
(defmacro twice (x)
  (list '+ x x))

;; Use the macro
(define four (twice 2))  ; Expands to (+ 2 2) = 4

;; Macro with conditional
(defmacro when (test body)
  (list 'if test body 'nil))

;; Use when macro
(define result 
  (when (> 5 3)
    "yes"))  ; Expands to (if (> 5 3) "yes" nil)

;; Testing macro expansion
(define test1 (twice 10))     ; 20
(define test2 (when #t 100))  ; 100
(define test3 (when #f 100))  ; nil