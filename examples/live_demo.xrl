;; Live Demo - True Liveness Test
;; 
;; This example demonstrates true liveness in XR-Lang.
;; You can modify this file while it's running and see changes
;; applied immediately without losing state!
;;
;; Try changing:
;; - The rotation speed in `spin-speed` 
;; - The cube positions in `make-cube`
;; - The colors or sizes
;; All changes will be applied live!

;; Create a camera
(define camera (create-camera [0 5 15]))

;; Define rotation speed - CHANGE THIS WHILE RUNNING!
(define spin-speed 90)  ; Try changing to 90, 180, etc.

;; Create a spinning cube - modify position while running!
(define make-cube
  (lambda (x y z)
    (let ((cube (create-cube [x y z])))
      (rotate cube [0 spin-speed 0])
      (scale cube [1.5 1.5 1.5])  ; Try changing scale!
      cube)))

;; Create multiple cubes in a row
(define cube1 (make-cube -4 0 0))
(define cube2 (make-cube 0 0 0))
(define cube3 (make-cube 4 0 0))

;; Add some spheres - modify this function while running!
(define add-sphere
  (lambda (x y z)
    (let ((sphere (create-sphere [x y z])))
      (scale sphere [0.8 0.8 0.8])  ; Try changing this!
      sphere)))

(define sphere1 (add-sphere -2 2 0))
(define sphere2 (add-sphere 2 2 0))

;; Animation counter - preserves state across reloads!
(define counter 0)
(define increment-counter
  (lambda ()
    (set! counter (+ counter 1))
    counter))

;; This will be called repeatedly and state will be preserved
;; even when you modify the code above!
(increment-counter)