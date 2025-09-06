; Comprehensive test of all 3D primitives with the new rendering pipeline
; Tests the complete flow: intrinsics -> VM -> scene extraction -> GPU rendering

; Camera setup
(create-camera [0 15 25] [0 0 0] 60)

; Row 1: Basic shapes with default parameters
(create-cube [-6 0 6])
(create-sphere [-3 0 6])
(create-cylinder [0 0 6])
(create-cone [3 0 6])
(create-pyramid [6 0 6])

; Row 2: Basic shapes with custom parameters
(create-sphere [-6 0 3] 0.8 16)          ; Lower poly sphere
(create-cylinder [-3 0 3] 0.6 2.5 8)     ; Octagonal cylinder
(create-cone [0 0 3] 0.7 2 12)           ; 12-sided cone
(create-pyramid [3 0 3] 1.5 1 2.5)       ; Tall pyramid
(create-wedge [6 0 3] 1.5 1.5 1.5)       ; Wedge

; Row 3: Advanced shapes
(create-torus [-6 0 0] 1 0.3 24 18)      ; Torus/donut
(create-plane [-3 0 -1] 2 2 4)           ; Small subdivided plane
(create-capsule [0 0 0] 0.4 2 16)        ; Capsule
(create-torus [3 0 0] 0.8 0.2 16 12)     ; Smaller torus
(create-plane [6 0 -1] 2 2 1)            ; Simple plane

; Row 4: Platonic solids with different sizes
(create-icosahedron [-6 0 -3] 0.8)
(create-octahedron [-3 0 -3] 0.8)
(create-tetrahedron [0 0 -3] 0.8)
(create-icosahedron [3 0 -3] 1.2)        ; Larger icosahedron
(create-octahedron [6 0 -3] 1.2)         ; Larger octahedron

; Row 5: Mix of primitives at different heights
(create-sphere [-6 3 0] 0.5 64)          ; High-poly floating sphere
(create-cylinder [-3 2 0] 0.3 4 32)      ; Tall thin cylinder
(create-cone [0 3 0] 0.5 1.5 24)         ; Floating cone
(create-torus [3 3 0] 0.6 0.15 32 24)    ; Floating thin torus
(create-capsule [6 2 0] 0.3 3 24)        ; Tall capsule

; Ground plane
(create-plane [0 -2 0] 20 20 1)