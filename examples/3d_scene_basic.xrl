;; Basic 3D Scene Example
;; Demonstrates scene primitives from Stage A

;; Create a camera positioned above and behind origin
(define camera (create-camera [0 5 20]))

;; Create some cubes at different positions
(define cube1 (create-cube [0 0 0]))
(define cube2 (create-cube [3 0 0]))
(define cube3 (create-cube [-3 0 0]))

;; Create spheres
(define sphere1 (create-sphere [0 2 0]))
(define sphere2 (create-sphere [2 2 -2]))

;; Transform objects
(update-transform cube2 [3 10 0])  ; Move cube2 up
(rotate cube1 [0 45 0])           ; Rotate cube1 around Y axis
(scale cube3 [2 0.5 2])           ; Scale cube3

;; Get position of an object
(define cube1-pos (get-position cube1))

;; Create a grid of cubes
(define make-grid
  (lambda (size)
    (define make-row
      (lambda (x z count)
        (if (< count size)
            (begin
              (create-cube [x 0 z])
              (make-row (+ x 2) z (+ count 1)))
            nil)))
    (define make-grid-helper
      (lambda (z count)
        (if (< count size)
            (begin
              (make-row (- 0 size) z 0)
              (make-grid-helper (+ z 2) (+ count 1)))
            nil)))
    (make-grid-helper (- 0 size) 0)))

;; Create a 3x3 grid
(make-grid 3)