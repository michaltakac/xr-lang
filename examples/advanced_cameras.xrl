;; Advanced Camera System Demo - Showcasing All Camera Features
;; This example demonstrates all the implemented camera APIs from the "FUTURE API IDEAS"

;; ============================================================================
;; PERSPECTIVE CAMERA WITH FULL CONTROL
;; ============================================================================

(define cam-perspective 
  (create-perspective-camera 
    :position [10 8 15]
    :target [0 0 0]
    :fov 75
    :up [0 1 0]
    :near 0.1
    :far 1000))

;; ============================================================================
;; ORTHOGRAPHIC CAMERA
;; ============================================================================

(define cam-ortho
  (create-orthographic-camera
    :position [0 30 0.1]
    :target [0 0 0]
    :size 20
    :near 0.1
    :far 100))

;; ============================================================================
;; ORBIT CAMERA
;; ============================================================================

(define cam-orbit
  (orbit-camera
    :target [0 0 0]
    :radius 15
    :theta 45
    :phi 30))

;; ============================================================================
;; FIRST-PERSON CAMERA
;; ============================================================================

(define cam-fps
  (create-fps-camera
    :position [0 1.8 10]
    :yaw 0
    :pitch 0
    :roll 0))

;; ============================================================================
;; CINEMATIC CAMERA WITH DOF
;; ============================================================================

(define cam-cinematic
  (create-cinematic-camera
    :position [8 3 12]
    :target [0 0 0]
    :fov 35
    :aperture 1.4
    :focus-distance 15))

;; ============================================================================
;; CREATE A SCENE TO VIEW
;; ============================================================================

;; Central focus object
(define center-object (create-cube [0 0 0]))
(scale center-object [3 3 3])
(rotate center-object [45 45 0])

;; Reference spheres at different positions
(define sphere-red (create-sphere [5 0 0]))
(define sphere-green (create-sphere [0 5 0]))
(define sphere-blue (create-sphere [0 0 5]))

;; Grid of cubes for spatial reference
(define make-grid
  (lambda (x-start x-end z-start z-end)
    (if (<= x-start x-end)
      (begin
        (if (<= z-start z-end)
          (begin
            (define c (create-cube [x-start 0 z-start]))
            (scale c [0.4 0.1 0.4])
            (make-grid x-start x-end (+ z-start 2) z-end))
          nil)
        (make-grid (+ x-start 2) x-end -6 z-end))
      nil)))

(make-grid -6 6 -6 6)

;; ============================================================================
;; THIRD-PERSON FOLLOW CAMERA
;; ============================================================================

;; Create a moving target for the follow camera
(define moving-target (create-sphere [3 1 3]))

(define cam-follow
  (create-follow-camera
    :target moving-target
    :offset [0 5 -10]
    :damping 0.1))

;; ============================================================================
;; CAMERA MANIPULATION FUNCTIONS
;; ============================================================================

;; Look-at demonstration
(look-at cam-perspective [0 2 0])

;; Move camera by offset
(move-camera cam-fps [0 0 -2])

;; Rotate camera
(rotate-camera cam-fps [0 30 0])

;; Zoom camera (adjusts FOV)
(zoom-camera cam-perspective 1.5)

;; Set specific FOV
(set-fov cam-cinematic 28)

;; ============================================================================
;; CAMERA UTILITIES
;; ============================================================================

;; Get camera properties
(define cam-pos (get-camera-position cam-perspective))
(define cam-target (get-camera-target cam-perspective))
(define cam-fov (get-camera-fov cam-perspective))

(println "Camera position:" cam-pos)
(println "Camera target:" cam-target)
(println "Camera FOV:" cam-fov)

;; ============================================================================
;; CAMERA SWITCHING
;; ============================================================================

;; Set active camera (uncomment to activate different cameras)
; (set-active-camera cam-perspective)  ; Perspective view
; (set-active-camera cam-ortho)        ; Top-down orthographic
; (set-active-camera cam-orbit)        ; Orbiting view
; (set-active-camera cam-fps)          ; First-person
; (set-active-camera cam-cinematic)    ; Cinematic with narrow FOV
; (set-active-camera cam-follow)       ; Following the moving target

;; Default to perspective camera
(set-active-camera cam-perspective)

;; ============================================================================
;; CAMERA EFFECTS
;; ============================================================================

;; Shake camera effect (for explosions, impacts, etc.)
; (shake-camera cam-fps 0.5 1.0)  ; intensity: 0.5, duration: 1.0 seconds

;; ============================================================================
;; INTERACTIVE CAMERA CYCLING
;; ============================================================================

;; Define a function to cycle through all cameras
(define cycle-all-cameras
  (lambda ()
    (println "Cycling to next camera...")
    (cycle-cameras)))

;; You can bind this to a key or button in your application
; (on-key "c" cycle-all-cameras)

;; ============================================================================
;; ANIMATED CAMERA DEMONSTRATION
;; ============================================================================

;; This would be called in an update loop to animate cameras
(define update-cameras
  (lambda (time)
    ;; Orbit camera animation
    (let ((new-theta (* time 10)))
      ; Would recreate orbit camera with new angle
      ; (set! cam-orbit (orbit-camera :target [0 0 0] :radius 15 :theta new-theta :phi 30))
      nil)
    
    ;; Follow camera tracking
    ; The follow camera automatically tracks its target
    
    ;; FPS camera controls would go here
    ; (on-input "w" (lambda () (move-camera cam-fps [0 0 1])))
    ; (on-input "s" (lambda () (move-camera cam-fps [0 0 -1])))
    ; (on-input "a" (lambda () (move-camera cam-fps [-1 0 0])))
    ; (on-input "d" (lambda () (move-camera cam-fps [1 0 0])))
    ))

;; ============================================================================
;; CAMERA STATE INFORMATION
;; ============================================================================

(println "")
(println "=== Advanced Camera System Demo ===")
(println "")
(println "Available Cameras:")
(println "1. cam-perspective - Perspective camera with full control")
(println "2. cam-ortho - Orthographic camera (top-down view)")
(println "3. cam-orbit - Orbiting camera")
(println "4. cam-fps - First-person camera")
(println "5. cam-cinematic - Cinematic camera with narrow FOV")
(println "6. cam-follow - Third-person follow camera")
(println "")
(println "Camera Functions:")
(println "- (set-active-camera camera-id) - Switch to a specific camera")
(println "- (cycle-cameras) - Cycle through all cameras")
(println "- (move-camera camera-id [dx dy dz]) - Move camera by offset")
(println "- (rotate-camera camera-id [rx ry rz]) - Rotate camera")
(println "- (zoom-camera camera-id factor) - Zoom in/out")
(println "- (look-at camera-id [x y z]) - Make camera look at point")
(println "- (shake-camera camera-id intensity duration) - Shake effect")
(println "")
(println "Camera Utilities:")
(println "- (get-camera-position camera-id)")
(println "- (get-camera-target camera-id)")
(println "- (get-camera-fov camera-id)")
(println "")

;; ============================================================================
;; VIEWPORT SUPPORT (Future Implementation)
;; ============================================================================

;; Multiple viewport support would allow split-screen and picture-in-picture
;; These are placeholders for future implementation:

;; Split screen example (not yet implemented):
; (create-viewport
;   :camera cam-perspective
;   :rect [0 0 0.5 1.0]      ; Left half of screen
;   :layer 0)

; (create-viewport
;   :camera cam-ortho
;   :rect [0.5 0 0.5 1.0]    ; Right half of screen
;   :layer 0)

;; Picture-in-picture example (not yet implemented):
; (create-pip-viewport
;   :camera cam-ortho
;   :size [200 150]
;   :position 'top-right
;   :border-color [1 1 1]
;   :opacity 0.8)

;; ============================================================================
;; NOTES
;; ============================================================================

;; This example demonstrates all the camera features that have been implemented
;; from the "FUTURE API IDEAS" section. The camera system now supports:
;;
;; 1. Multiple camera types (perspective, orthographic, orbit, FPS, cinematic, follow)
;; 2. Full camera control (position, target, FOV, up vector, near/far planes)
;; 3. Camera manipulation (move, rotate, zoom, look-at)
;; 4. Camera utilities (get position, target, FOV)
;; 5. Camera switching (set active, cycle through cameras)
;; 6. Camera effects (shake)
;;
;; The viewport support is noted but not yet fully implemented, as it would
;; require additional renderer support for multiple viewports.
;;
;; All these features follow the XR-Lang vision of homoiconicity and live
;; programming, where cameras are first-class objects that can be manipulated
;; and inspected at runtime.