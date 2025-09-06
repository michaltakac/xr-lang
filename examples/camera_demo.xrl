;; Camera API Demo - Multiple Cameras and Switching
;; 
;; XR-Lang supports multiple cameras, but only one is active at a time.
;; The renderer uses the last camera defined with (define camera ...)

;; Create multiple cameras with different perspectives
(define cam-front (create-camera [0 5 15]))
(define cam-top (create-camera [0 25 0.1] [0 0 0]))  ; Looking down
(define cam-side (create-camera [15 5 0] [0 0 0]))   ; From the side
(define cam-iso (create-camera [10 10 10] [0 0 0] 45)) ; Isometric-ish view

;; Create a scene to view from different angles
(define center-cube (create-cube [0 0 0]))
(scale center-cube [2 2 2])
(rotate center-cube [45 0 45])

;; Create reference objects at each axis
(define x-sphere (create-sphere [5 0 0]))  ; Red sphere on X axis
(define y-sphere (create-sphere [0 5 0]))  ; Green sphere on Y axis  
(define z-sphere (create-sphere [0 0 5]))  ; Blue sphere on Z axis

;; Grid of small cubes for spatial reference
(define make-grid-plane
  (lambda ()
    (define make-grid-row
      (lambda (x z)
        (if (< x 5)
          (begin
            (define c (create-cube [x 0 z]))
            (scale c [0.3 0.1 0.3])
            (make-grid-row (+ x 2) z))
          nil)))
    (define make-grid
      (lambda (z)
        (if (< z 5)
          (begin
            (make-grid-row -4 z)
            (make-grid (+ z 2)))
          nil)))
    (make-grid -4)))

(make-grid-plane)

;; ACTIVE CAMERA SELECTION
;; Uncomment the camera you want to use:
; (define camera cam-front)  ; Default front view
; (define camera cam-top)    ; Top-down view
; (define camera cam-side)   ; Side view
 (define camera cam-iso)    ; Isometric view

;; ============================================================================
;; COMPREHENSIVE CAMERA API DOCUMENTATION
;; ============================================================================

;; Current API (Implemented):
;; --------------------------
;; Position only (looks at origin):
;; (create-camera [x y z])

;; Position and target:
;; (create-camera [x y z] [target-x target-y target-z])

;; Position, target, and FOV:
;; (create-camera [x y z] [target-x target-y target-z] fov-degrees)

;; Examples:
;; (create-camera [0 5 15])           ; Simple position
;; (create-camera [10 10 10] [0 0 0]) ; Position + look at origin
;; (create-camera [0 8 20] [0 0 0] 45) ; Position + target + 45° FOV

;; ============================================================================
;; FUTURE API IDEAS (To Be Implemented)
;; ============================================================================

;; Perspective Camera with Full Control (Three.js-style)
;; ------------------------------------------------------
;; (create-perspective-camera
;;   :position [0 5 15]
;;   :target [0 0 0]      ; Look-at point
;;   :fov 75              ; Vertical field of view in degrees
;;   :aspect 1.6          ; Aspect ratio (width/height)
;;   :near 0.1            ; Near clipping plane
;;   :far 1000            ; Far clipping plane
;;   :up [0 1 0])         ; Up vector for orientation

;; Orthographic Camera for Isometric/CAD Views
;; --------------------------------------------
;; (create-orthographic-camera
;;   :position [10 10 10]
;;   :target [0 0 0]
;;   :left -10            ; Left frustum boundary
;;   :right 10            ; Right frustum boundary
;;   :top 10              ; Top frustum boundary
;;   :bottom -10          ; Bottom frustum boundary
;;   :near 0.1            ; Near clipping plane
;;   :far 100             ; Far clipping plane
;;   :zoom 1.0)           ; Zoom factor

;; Camera with Rotation (Euler Angles or Quaternion)
;; --------------------------------------------------
;; (create-camera 
;;   :position [0 5 15]
;;   :rotation [0 30 0]   ; Euler angles in degrees (yaw, pitch, roll)
;;   :fov 60)

;; Or with quaternion:
;; (create-camera
;;   :position [0 5 15]
;;   :quaternion [0 0.707 0 0.707]  ; Quaternion [x y z w]
;;   :fov 60)

;; Look-At Helper (Updates Camera to Look at Point)
;; -------------------------------------------------
;; (look-at camera-id [x y z])
;; (look-at camera-id target-object-id)  ; Look at another object

;; Example:
;; (look-at cam-front [0 2 0])           ; Look at position
;; (look-at cam-front moving-sphere)      ; Track an object

;; Orbit Camera (Spherical Coordinates)
;; ------------------------------------
;; (orbit-camera
;;   :target [0 0 0]      ; Center of orbit
;;   :radius 10           ; Distance from target
;;   :theta 45            ; Horizontal angle in degrees
;;   :phi 30              ; Vertical angle in degrees
;;   :up [0 1 0])         ; Up vector

;; Or create with:
;; (create-orbit-camera
;;   :target [0 0 0]
;;   :distance 15
;;   :azimuth 45          ; Alternative naming
;;   :elevation 30)

;; First-Person Camera
;; -------------------
;; (create-fps-camera
;;   :position [0 1.8 0]  ; Eye height
;;   :yaw 0               ; Horizontal rotation
;;   :pitch 0             ; Vertical rotation
;;   :roll 0)             ; Tilt

;; Third-Person Camera (Follow Camera)
;; -----------------------------------
;; (create-follow-camera
;;   :target player-object
;;   :offset [0 5 -10]    ; Offset from target
;;   :damping 0.1         ; Smooth following
;;   :look-ahead 2.0)     ; Look ahead of movement

;; Cinematic Camera with Depth of Field
;; -------------------------------------
;; (create-cinematic-camera
;;   :position [0 5 15]
;;   :target [0 0 0]
;;   :fov 35              ; Narrower FOV for cinematic look
;;   :aperture 1.4        ; f-stop for DOF
;;   :focus-distance 15   ; Distance to focus plane
;;   :bokeh-scale 2.0)    ; Bokeh effect intensity

;; ============================================================================
;; CAMERA MANIPULATION FUNCTIONS
;; ============================================================================

;; Set Active Camera at Runtime
;; -----------------------------
;; (set-active-camera camera-id)
;; (cycle-cameras)                        ; Cycle through all cameras
;; (next-camera)                          ; Switch to next camera
;; (previous-camera)                     ; Switch to previous camera

;; Camera Movement
;; ---------------
;; (move-camera camera-id [dx dy dz])    ; Relative movement
;; (rotate-camera camera-id [rx ry rz])  ; Relative rotation
;; (zoom-camera camera-id factor)        ; Zoom in/out

;; Camera Animation
;; ----------------
;; (animate-camera camera-id
;;   :to-position [10 5 10]
;;   :to-target [0 0 0]
;;   :duration 2.0                       ; Seconds
;;   :easing 'ease-in-out)                ; Animation curve

;; Camera Shake Effect
;; -------------------
;; (shake-camera camera-id
;;   :intensity 0.5
;;   :duration 0.5
;;   :frequency 10)                      ; Shakes per second

;; ============================================================================
;; CAMERA UTILITIES
;; ============================================================================

;; Get Camera Properties
;; ----------------------
;; (get-camera-position camera-id)       ; Returns [x y z]
;; (get-camera-target camera-id)         ; Returns [x y z]
;; (get-camera-fov camera-id)            ; Returns degrees
;; (get-camera-matrix camera-id)         ; Returns view matrix

;; Screen/World Coordinate Conversion
;; ----------------------------------
;; (screen-to-world camera-id [screen-x screen-y])
;; (world-to-screen camera-id [world-x world-y world-z])

;; Frustum Culling Check
;; ----------------------
;; (in-camera-view? camera-id object-id)
;; (get-visible-objects camera-id)

;; ============================================================================
;; MULTIPLE VIEWPORT SUPPORT (Future)
;; ============================================================================

;; Split Screen
;; ------------
;; (create-viewport
;;   :camera cam-top
;;   :rect [0 0 0.5 0.5]                 ; Top-left quarter
;;   :layer 0)

;; (create-viewport
;;   :camera cam-front
;;   :rect [0.5 0 0.5 1.0]                ; Right half
;;   :layer 1)

;; Picture-in-Picture
;; ------------------
;; (create-pip-viewport
;;   :camera minimap-cam
;;   :size [200 150]                     ; Pixels
;;   :position 'top-right
;;   :border-color [1 1 1]
;;   :opacity 0.8)

;; ============================================================================
;; CAMERA PRESERVATION NOTES
;; ============================================================================

;; Camera State Preservation:
;; 1. Camera positions are automatically preserved in Play mode
;; 2. When you move a camera with WASD/arrows/mouse, the position persists
;; 3. Live code changes maintain camera state
;; 4. Press 'P' to cycle preservation modes:
;;    - Play: Preserve camera position (default)
;;    - Live: Preserve and sync to code
;;    - Design: Reset to script position

;; Try editing camera parameters while running!
;; The camera position will be preserved if you've moved it with WASD/mouse