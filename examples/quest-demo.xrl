;; XR-Lang Quest 3 Demo Scene
;; This scene demonstrates hot-reload and XR features

(defscene3d quest-demo
  ;; Camera positioned for VR viewing
  (camera 
    (position 0 1.6 0)  ; Eye level height
    (meta preserve-runtime))  ; Keep position during hot-reload
  
  ;; Ground plane
  (plane
    (position 0 0 0)
    (scale 10 1 10)
    (material :color "#2a2a2a"))
  
  ;; Interactive cube that responds to controller input
  (cube
    (id "main-cube")
    (position 0 1.5 -2)
    (scale 0.5 0.5 0.5)
    (material :color "#00ff00")
    (meta preserve-runtime)  ; Preserve transforms during hot-reload
    (behavior
      ;; Rotate continuously
      (on-update [dt]
        (set! rotation.y (+ rotation.y (* dt 45))))
      ;; Change color on controller trigger
      (on-trigger []
        (set! material.color (random-color)))))
  
  ;; Floating spheres arranged in a circle
  (dotimes [i 8]
    (let [angle (* i (/ (* 2 Math/PI) 8))
          x (* 3 (cos angle))
          z (* 3 (sin angle))]
      (sphere
        (id (str "sphere-" i))
        (position x 2 z)
        (scale 0.3 0.3 0.3)
        (material 
          :color (hsl (* i 45) 0.7 0.5)
          :metallic 0.8
          :roughness 0.2)
        (behavior
          ;; Bob up and down
          (on-update [dt]
            (set! position.y (+ 2 (* 0.5 (sin (* time 2))))))))))
  
  ;; Text label in 3D space
  (text3d
    (content "XR-Lang on Quest 3")
    (position 0 3 -2)
    (scale 0.1 0.1 0.1)
    (material :color "#ffffff"))
  
  ;; Hand tracking indicators (when available)
  (hand-tracker
    (id "left-hand")
    (hand :left)
    (behavior
      (on-pinch []
        (spawn-particle-burst position))))
  
  (hand-tracker
    (id "right-hand")
    (hand :right)
    (behavior
      (on-pinch []
        (spawn-particle-burst position))))
  
  ;; Passthrough toggle (Quest specific)
  (behavior
    (on-key "P"
      (toggle-passthrough)))
  
  ;; Performance display
  (ui-overlay
    (position :top-left)
    (text (str "FPS: " (get-fps)))
    (text (str "Frame: " (get-frame-time) "ms"))
    (text "Hot-reload enabled on port 9090")))

;; Helper function for random colors
(defn random-color []
  (hsl (random 360) 0.7 0.5))

;; Helper to spawn particles
(defn spawn-particle-burst [pos]
  (particles
    (position pos)
    (count 50)
    (lifetime 1.0)
    (velocity (random-sphere 2))
    (color-gradient ["#ff0000" "#ffff00" "#0000ff"])
    (size-curve [0.1 0.05 0.0])))

;; Toggle passthrough mode
(defn toggle-passthrough []
  (let [current (get-capability :passthrough)]
    (set-capability :passthrough (not current))
    (if current
      (log "Passthrough disabled")
      (log "Passthrough enabled"))))