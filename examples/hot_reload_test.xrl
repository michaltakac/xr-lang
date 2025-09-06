;; Hot-reload test - modify camera position and object transforms
;; Try changing the camera position values and saving the file!

;; Camera with position, target, and FOV - CHANGE THESE VALUES to test hot-reload
;; (create-camera position target fov-in-degrees)
(define camera (create-camera [0 10 30] [0 0 0] 60))  ; Try changing position, target, or FOV!

;; Create test objects
(define cube1 (create-cube [0 0 0]))
(define cube2 (create-cube [5 0 0]))
(define cube3 (create-cube [-5 0 0]))

;; Transform objects - CHANGE THESE to test hot-reload
(rotate cube1 [0 45 0])     ; Try different angles
(scale cube2 [2 2 2])       ; Try different scales
(scale cube3 [1 3 1])       ; Try different scales

;; Add some spheres
(define sphere1 (create-sphere [0 3 0]))
(define sphere2 (create-sphere [3 3 0]))

;; Transform spheres - CHANGE THESE to test hot-reload
(scale sphere1 [1.5 1.5 1.5])
(update-transform sphere2 [3 5 0])  ; Try different positions