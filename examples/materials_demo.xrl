;; Materials Demo - showcase basic, standard, lambert, phong, toon

;; Camera
(define cam (create-camera [0 5 12] [0 0 0]))

;; Basic (unlit), static color
(define basic (cube [-4 0 0]))
(material basic {:type "basic" :color "#ff8844" :animated false})

;; Standard (PBR-like)
(define standard (sphere [-2 0 0]))
(material standard {:type "standard" :base_color [0.2 0.7 1.0 1.0] :metallic 0.3 :roughness 0.6})

;; Lambert (diffuse)
(define lam (wedge [0 0 0] 2 1 1))
(material lam {:type "lambert" :color "orange"})

;; Phong (specular)
(define pho (cylinder [2 0 0] 0.6 1.6))
(material pho {:type "phong" :color [0.3 0.8 0.9 1.0] :shininess 64 :specular [1 1 1]})

;; Toon (cel-shaded)
(define toon (cone [4 0 0] 0.6 1.5 24))
(material toon {:type "toon" :color "magenta" :levels 5})

;; Plane
(define ground (plane [0 -1 0] 14 6 2))
(material ground {:type "basic" :color "#222222" :animated false})
