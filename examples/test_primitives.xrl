; Simple test of primitive rendering with parameters
; This tests the full pipeline from intrinsics to GPU rendering

; Camera
(create-camera [0 5 10] [0 0 0] 60)

; Basic cube at origin
(create-cube [0 0 0])

; Sphere with custom radius and segments
(create-sphere [3 0 0] 1.5 32)

; Cylinder with custom dimensions
(create-cylinder [-3 0 0] 0.8 2.5 24)

; Torus (donut) with custom radii
(create-torus [0 0 -3] 1.2 0.4 24 18)

; Cone
(create-cone [3 0 -3] 1 2 16)

; Pyramid
(create-pyramid [-3 0 -3] 1.5 1.5 2)