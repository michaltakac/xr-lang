; XR-Lang Stage A Demo - Homoiconic Foundation
; Demonstrates the core features implemented in Stage A:
; 1. EDN-like S-expression syntax with rich data types
; 2. Homoiconic value model (code as data)
; 3. Scene primitives as intrinsic functions
; 4. Persistence with time-travel capability

; Define a simple 3D scene using S-expressions
(defscene3d demo-world
  ; Camera with metadata for runtime preservation
  (camera 
    (position [0 5 10])
    ^{:preserve-runtime true}  ; Metadata annotation
    (target [0 0 0]))
  
  ; Create a cube with behavior
  (cube :my-cube
    (position [0 0 0])
    (color {:r 1.0 :g 0.0 :b 0.0})
    (behavior rotate-on-click))
  
  ; Create a sphere
  (sphere :my-sphere
    (position [3 0 0])
    (scale [1.5 1.5 1.5]))
  
  ; Lighting configuration
  (lighting
    (ambient 0.2)
    (directional {:direction [1 -1 -1] :intensity 0.8})))

; Define a behavior as data (homoiconic!)
(defbehavior rotate-on-click
  ; State is just data
  {:rotation 0.0}
  
  ; Event handlers are code as data
  (on-click [self event]
    (update-state self :rotation 
      (fn [r] (+ r 45))))
  
  (on-frame [self dt]
    (rotate self [0 (get-state self :rotation) 0])))

; Demonstrate macros that write code
(defmacro with-time-travel [& body]
  `(let [checkpoint# (journal-checkpoint)]
     (try
       ~@body
       (catch Exception e#
         (journal-travel-to checkpoint#)
         (throw e#)))))

; Example of meta-programming: generate behaviors
(defmacro generate-oscillator [property amplitude frequency]
  `(defbehavior ~(symbol (str "oscillate-" property))
     {:time 0.0}
     (on-frame [self# dt#]
       (update-state self# :time (fn [t#] (+ t# dt#)))
       (set-property self# ~property
         (* ~amplitude (sin (* ~frequency (get-state self# :time))))))))

; Generate different oscillator behaviors
(generate-oscillator :position-y 2.0 1.0)
(generate-oscillator :rotation-y 180.0 0.5)

; Demonstrate homoiconicity - code that examines itself
(defn analyze-scene [scene-form]
  ; Parse the scene definition as data
  (let [scene-name (second scene-form)
        elements (drop 2 scene-form)]
    {:name scene-name
     :cameras (filter #(= 'camera (first %)) elements)
     :objects (filter #(contains? #{'cube 'sphere} (first %)) elements)
     :count (count elements)}))

; Quote the scene definition to treat it as data
(def scene-data '(defscene3d demo-world ...))

; Analyze the scene structure
(println (analyze-scene scene-data))

; Time-travel example
(with-time-travel
  ; Create objects
  (def obj1 (create-cube [0 0 0]))
  (def obj2 (create-sphere [5 0 0]))
  
  ; Record state changes
  (journal-record! :move-cube 
    (update-transform obj1 [10 0 0]))
  
  ; Travel back if needed
  (when (> (distance obj1 obj2) 15)
    (journal-travel-back 1)))

; Demonstrate persistence policies
(def game-state
  {:player-position [0 0 0]       ; Volatile - resets on reload
   ^:preserve-runtime              ; Preserved across hot-reloads
   :camera-position [0 5 10]       
   ^:sync-to-code                  ; Synced back to source code
   :level-layout [...]})