; XR-Lang 3D Primitives Showcase
; Demonstrates all supported 3D primitive types
; Following Stage A requirement #5: Scene Primitives and 3D Model Loading

; Create a camera to view the scene
(def camera (create-camera [0 10 20] [0 0 0] 60))

; Basic Primitives
; ================

; Cube/Box - The classic
(def cube1 (create-cube [0 0 0]))

; Sphere with custom radius and segments
(def sphere1 (create-sphere [-5 0 0] 1.5 32))

; Cylinder with custom dimensions
(def cylinder1 (create-cylinder [5 0 0] 1 3 24))

; Cone pointing upward
(def cone1 (create-cone [-5 0 5] 1 2 16))

; Pyramid with rectangular base
(def pyramid1 (create-pyramid [0 0 5] 2 2 3))

; Wedge (triangular prism)
(def wedge1 (create-wedge [5 0 5] 2 2 2))

; Advanced Primitives
; ===================

; Torus (donut shape)
(def torus1 (create-torus [-5 0 -5] 1.5 0.5 24 18))

; Plane for ground
(def ground (create-plane [0 -2 0] 20 20 10))

; Capsule (cylinder with rounded caps)
(def capsule1 (create-capsule [0 0 -5] 0.5 2 16))

; Platonic Solids
; ================

; Icosahedron (20 faces)
(def icosa1 (create-icosahedron [5 0 -5] 1.5))

; Octahedron (8 faces)
(def octa1 (create-octahedron [-10 0 0] 1.5))

; Tetrahedron (4 faces)
(def tetra1 (create-tetrahedron [10 0 0] 1.5))

; Transform Examples
; ==================

; Rotate the torus
(rotate torus1 [0 45 0])

; Scale the pyramid
(scale pyramid1 [1 2 1])

; Move the sphere up
(update-transform sphere1 [-5 2 0])

; Create a grid of cubes to show positioning
(def grid-cubes
  (for [x (range -10 10 5)
        z (range -10 10 5)]
    (create-cube [x 5 z])))

; Advanced positioning with different primitives
(def showcase-row
  (list
    (create-sphere [-8 8 -10] 0.5 16)
    (create-cylinder [-6 8 -10] 0.5 1 12)
    (create-cone [-4 8 -10] 0.5 1 12)
    (create-pyramid [-2 8 -10] 1 1 1)
    (create-wedge [0 8 -10] 1 1 1)
    (create-torus [2 8 -10] 0.5 0.2 12 8)
    (create-capsule [4 8 -10] 0.3 1 12)
    (create-icosahedron [6 8 -10] 0.5)
    (create-octahedron [8 8 -10] 0.5)))

; Function to create a primitive based on type
(defn create-primitive [type pos]
  (case type
    :cube (create-cube pos)
    :sphere (create-sphere pos 1 24)
    :cylinder (create-cylinder pos 1 2 24)
    :cone (create-cone pos 1 2 24)
    :pyramid (create-pyramid pos 2 2 2)
    :wedge (create-wedge pos 2 2 2)
    :torus (create-torus pos 1 0.3 24 18)
    :plane (create-plane pos 10 10 5)
    :capsule (create-capsule pos 0.5 2 24)
    :icosahedron (create-icosahedron pos 1)
    :octahedron (create-octahedron pos 1)
    :tetrahedron (create-tetrahedron pos 1)
    (create-cube pos))) ; default

; Example usage of the function
(def my-shape (create-primitive :torus [0 3 8]))

; Creating variations with different parameters
(def sphere-variations
  (list
    (create-sphere [-3 6 0] 0.5 8)   ; Low poly sphere
    (create-sphere [0 6 0] 0.5 16)   ; Medium poly sphere  
    (create-sphere [3 6 0] 0.5 64))) ; High poly sphere

(def cylinder-variations
  (list
    (create-cylinder [-3 6 3] 0.5 1 6)   ; Hexagonal cylinder
    (create-cylinder [0 6 3] 0.5 2 8)    ; Octagonal cylinder
    (create-cylinder [3 6 3] 0.3 3 32))) ; Smooth tall cylinder

; Print confirmation
(println "All 3D primitives created successfully!")
(println "Camera positioned to view the scene")
(println "Total primitives created: numerous!")