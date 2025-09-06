;; Simple Visual Demo - Basic 3D Shapes
;; Shows immediate visual output with basic operations

;; Setup camera to see the scene
(define camera (create-camera [0 5 10]))

;; Create a red cube in the center
(define center-cube (create-cube [0 0 0]))
(scale center-cube [2 2 2])

;; Create blue spheres orbiting around
(define left-sphere (create-sphere [-3 0 0]))
(define right-sphere (create-sphere [3 0 0]))

;; Rotate the center cube for visual effect  
(rotate center-cube [0 45 45])