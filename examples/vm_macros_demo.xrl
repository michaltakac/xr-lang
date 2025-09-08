;; VM Macro Demo - Basic 3D Scene using XR-Lang macros

;; Camera
(define camera (create-camera [0 5 10] [0 0 0]))

;; Center cube and transforms
(define cube (cube [0 0 0]))
(move cube [1 0 0])
(rotate cube [0 45 0])
(scale cube [1.5 1.5 1.5])
(color cube "#cbff0eff")

;; More primitives
(sphere [3 0 0])
(cylinder [0 1 0] 0.5 2)
(cone [0 0 3] 0.5 1.5 24)
(pyramid [-3 0 0] 2 2 2)
(wedge [0 0 -13] 2 1 1)
(torus [2 0 -2] 2 0.5 24 16)
(plane [0 -1 0] 10 10 4)
