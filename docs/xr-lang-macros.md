XR-Lang Scene Macros (VM Evaluator)

This shows a simple 3D scene using XR‑Lang macros executed by the VM evaluator (not the DSL parser). Macros are thin wrappers over native intrinsics in `vm::intrinsics`.

Quick example (see `examples/vm_macros_demo.xrl`):

```
;; Camera
(define cam (create-camera [0 5 10] [0 0 0]))

;; Objects
(define cube (cube [0 0 0]))
(move cube [1 0 0])
(rotate cube [0 45 0])
(scale cube [1.5 1.5 1.5])

(sphere [3 0 0])
(cylinder [0 1 0] 0.5 2)
(plane [0 -1 0] 10 10 4)
```

Available macros (selection):
- Primitives: `cube pos`, `sphere pos`, `cylinder pos radius height`, `cone pos radius height segments`, `pyramid pos basew based h`, `wedge pos w h d`, `torus pos R r seg rings`, `plane pos w h sub`, `capsule pos radius height seg`, `icosahedron pos radius`, `octahedron pos radius`, `tetrahedron pos radius`
- Camera: `camera pos target`, `camera-fov pos target fov`
- Transform helper: `move obj pos` (use native `rotate`, `scale` intrinsics directly)
- Material helper: `color obj [r g b a]` (stores RGBA on node; renderer integration TBD)

Notes:
- VM-level macros are distinct from `.xrl` DSL examples that use the higher-level parser.
- Materials and advanced scene directives remain in the DSL layer; VM macros focus on geometry/transforms.
