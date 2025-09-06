;; HOT-RELOAD DEMONSTRATION
;; ========================
;; Save this file after making changes to see instant updates!
;; Everything updates without losing runtime state in Design mode.

;; CAMERA SETTINGS - All parameters are hot-reloadable!
;; -----------------------------------------------------
;; Try different camera positions, targets, and field of view:
(define camera (create-camera 
    [10 15 25]     ; position: try [0 10 30], [20 5 20], [-10 20 15]
    [0 0 0]        ; target: try [5 0 0], [0 5 0], [-5 2 -5]  
    75))           ; FOV in degrees: try 45, 60, 90, 120

;; CUBE ARRANGEMENT - Hot-reload positions and transforms!
;; --------------------------------------------------------
(define center-cube (create-cube [0 0 0]))
(rotate center-cube [45 45 0])     ; Try different rotations!
(scale center-cube [2 2 2])        ; Try [1 3 1], [3 1 3], etc.

;; Ring of cubes - change positions dynamically
(define cube-1 (create-cube [5 0 0]))
(define cube-2 (create-cube [0 0 5]))
(define cube-3 (create-cube [-5 0 0]))
(define cube-4 (create-cube [0 0 -5]))

;; Apply different scales to ring cubes
(scale cube-1 [1.5 1.5 1.5])   ; Change these values!
(scale cube-2 [1 2 1])
(scale cube-3 [2 1 2])  
(scale cube-4 [1.2 1.2 1.2])

;; SPHERE ARRANGEMENT - Hot-reload sphere properties!
;; ---------------------------------------------------
(define sphere-1 (create-sphere [0 5 0]))
(scale sphere-1 [2 2 2])           ; Big sphere on top

(define sphere-2 (create-sphere [3 3 3]))
(define sphere-3 (create-sphere [-3 3 -3]))
(define sphere-4 (create-sphere [3 3 -3]))
(define sphere-5 (create-sphere [-3 3 3]))

;; DYNAMIC POSITIONING - Change these values and save!
;; ----------------------------------------------------
(update-transform sphere-2 [4 4 4])    ; Try different positions
(update-transform sphere-3 [-4 4 -4])
(update-transform sphere-4 [4 4 -4])
(update-transform sphere-5 [-4 4 4])

;; ROTATION GALLERY - See instant rotation updates!
;; -------------------------------------------------
(define rot-cube-1 (create-cube [8 0 0]))
(rotate rot-cube-1 [0 0 45])      ; Z rotation

(define rot-cube-2 (create-cube [10 0 0]))
(rotate rot-cube-2 [45 0 0])      ; X rotation

(define rot-cube-3 (create-cube [12 0 0]))
(rotate rot-cube-3 [0 45 0])      ; Y rotation

;; TIPS FOR TESTING:
;; -----------------
;; 1. Change any number and save - see instant updates!
;; 2. Camera changes apply immediately
;; 3. All transforms (position, rotation, scale) hot-reload
;; 4. Add or remove objects - scene rebuilds automatically
;; 5. Try extreme values to see the limits!