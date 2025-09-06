;; Basic 3D Scene - Visual Primitives
;; Creates a simple scene with cubes and spheres you can see

;; Camera positioned to view the scene
(define camera (create-camera [0 5 20]))

;; Create three cubes in a row - you'll see them lined up
(define left-cube (create-cube [-4 0 0]))
(define center-cube (create-cube [0 0 0]))
(define right-cube (create-cube [4 0 0]))

;; Visual transformations you can see:
(rotate center-cube [0 45 0])     ; Center cube rotated 45 degrees
(scale right-cube [2 0.5 2])      ; Right cube flattened
(update-transform left-cube [-4 2 0]) ; Left cube floats up

;; Add spheres above - creates a visual hierarchy
(define sphere1 (create-sphere [0 3 0]))
(define sphere2 (create-sphere [-2 3 0]))
(scale sphere1 [1.5 1.5 1.5])     ; Bigger sphere in center