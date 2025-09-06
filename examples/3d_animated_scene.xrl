;; Animated 3D Scene Example
;; Shows how to create dynamic scenes with transformations

;; Setup camera
(define main-camera (create-camera [0 10 20]))

;; Create a central rotating cube
(define center-cube (create-cube [0 0 0]))
(scale center-cube [2 2 2])

;; Create orbiting spheres
(define orbit-sphere-1 (create-sphere [5 0 0]))
(define orbit-sphere-2 (create-sphere [-5 0 0]))
(define orbit-sphere-3 (create-sphere [0 0 5]))

;; Function to animate rotation
(define animate-rotation
  (lambda (object angle-x angle-y angle-z)
    (rotate object [angle-x angle-y angle-z])))

;; Function to orbit an object around origin
(define orbit
  (lambda (object radius angle height)
    (let ((x (* radius (cos angle)))
          (z (* radius (sin angle))))
      (update-transform object [x height z]))))

;; Animation helpers (would be called each frame in real implementation)
(define update-frame
  (lambda (time)
    ;; Rotate center cube
    (animate-rotation center-cube 
                     (* time 30)
                     (* time 45) 
                     (* time 20))
    
    ;; Orbit spheres
    (orbit orbit-sphere-1 5 time 0)
    (orbit orbit-sphere-2 5 (+ time 2.094) 0)  ; 120 degrees offset
    (orbit orbit-sphere-3 5 (+ time 4.188) 0)))  ; 240 degrees offset

;; Create a pyramid of cubes
(define build-pyramid
  (lambda (levels)
    (define build-level
      (lambda (level size)
        (if (< level levels)
            (let ((y (* level 1.5))
                  (offset (/ size 2)))
              (define place-cubes
                (lambda (x z count)
                  (if (< count (* size size))
                      (begin
                        (define cube (create-cube [x y z]))
                        (scale cube [0.9 0.9 0.9])
                        (place-cubes 
                          (if (< (+ x 1) size)
                              (+ x 1)
                              0)
                          (if (< (+ x 1) size)
                              z
                              (+ z 1))
                          (+ count 1)))
                      nil)))
              (place-cubes (- 0 offset) (- 0 offset) 0)
              (build-level (+ level 1) (- size 1)))
            nil)))
    (build-level 0 levels)))

;; Build a 4-level pyramid
(build-pyramid 4)

;; Simulate one frame of animation at time = 1.0
(update-frame 1.0)