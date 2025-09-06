;; Simple Camera Test

;; Create a perspective camera
(define cam1 (create-perspective-camera 
  :position [5 5 10]
  :target [0 0 0]
  :fov 60))

;; Create a simple scene
(define cube1 (create-cube [0 0 0]))
(scale cube1 [2 2 2])

;; Set active camera
(set-active-camera cam1)

;; Test camera utilities
(define pos (get-camera-position cam1))
(define target (get-camera-target cam1))
(define fov (get-camera-fov cam1))

(println "Camera position:" pos)
(println "Camera target:" target)
(println "Camera FOV:" fov)

;; Move the camera
(move-camera cam1 [0 2 0])
(println "After move, position:" (get-camera-position cam1))