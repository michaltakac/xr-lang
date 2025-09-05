# XR-lang Design Review

## About the project

We’re synthesizing Lisp/Smalltalk-style *self-describing worlds* with modern XR/graphics and agentic AI. Read as a language person, the core is solid: homoiconicity, image-style development, reflective runtime, and a principled stance on hot-reload vs. state. Below is how we could shape it so it actually lands.

## Short Design Review

### What you must not miss (language/VM level)

1. **A crisp object model and state model before syntax.**
   Decide early: object system = prototypes (Self-like) or classes (Smalltalk-like) or pure data + behaviors (ECS). Your three-layer state is great, but it needs a *formal* patch/merge model. Define:

   * A **typed value graph** (scene graph + behavior graph).
   * A **diff/patch language** for that graph (semantic, not textual).
   * A **precedence lattice**: Source < RuntimeOverride < LiveMutation, with conflict tags & provenance.

2. **Image + journals, not just files.**
   Keep the “image” (active object memory) as the ground truth. Persist via **event sourcing**:

   * Immutable **timeline of changes** (journals), plus periodic **snapshots**.
   * “Replay” == reapply journals onto snapshot. Your time-travel then becomes trivial, causality comes from journal linkages.

3. **Homoiconic core with hygienic macros + pattern matching.**
   Don’t ship a macro system without: hygiene, quasiquote/unquote-splicing, and **pattern-matching macros** (think `syntax-rules` + `syntax-case` or Racket match expanders). It’s where 80% of the power lives.

4. **A metacircular tower that bottoms out cleanly.**

   * Stage 0: tiny evaluator (Rust).
   * Stage 1: XR-Lang interpreter in XR-Lang (metacircular), proven by bootstrapping.
   * Stage 2: partial evaluator/compiler that emits bytecode.
   * Stage 3: JIT (optional/later).

5. **Deterministic islands with escape hatches.**
   Rendering/physics/IO are non-deterministic; metaprogramming and replay need determinism. Provide:

   * **Pure compute sandboxes** (deterministic, replayable).
   * **Effect capabilities** for non-deterministic ops, logged in journals for time-travel.

6. **Capability-based security.**
   If the language self-modifies, authority must be explicit. Capabilities for file write, network, GPU, AI calls; nothing ambient.

7. **Probes instead of printf.**
   Build **probes/watchpoints** into the core (Victor’s “show the data”): any value/collection/stream can expose a live view without editing user code (attach probes at the meta layer).

### What to put in Rust vs. what to put in XR-Lang (bootstrapping split)

Think in **rings**. Inner rings are Rust; outer rings migrate into XR-Lang as soon as the system can carry itself.

**Rust (stable substrate, \~35–45% of the initial system)**

* **Runtime kernel**: memory mgmt for the image, arenas, persistence (snapshot + journal), GC (or RC + cycle detection initially).
* **Scheduler**: fibers/green threads, prioritized frames (render/update/AI).
* **Bytecode VM** (simple, register-based or stack; pick one and keep it tiny).
* **FFI & capability system** (host ↔ language boundary).
* **Scene/ECS & renderer bindings**: minimal draw/mesh/material API, async loaders.
* **I/O, audio, XR device layer**, clock/timers.
* **Hot-reload loader + patch applier** (reads DSL/AST and merges via your precedence rules).
* **Minimal standard library intrinsics** (math, collections, byte arrays).

**XR-Lang (everything you want to hack live, \~55–65% as soon as possible)**

* Reader/printer, macro expander (hygienic), module system.
* Metacircular evaluator, then compiler to bytecode.
* Source rewriter, code-gen helpers, “meta behaviors”.
* Inspector/console/profiler UIs (written in the language, living in the world).
* High-level scene DSL, behaviors, shader graph compiler (to WGSL/GLSL via Rust intrinsics).
* AI “first-class agent” facades (tool functions call out to host via capabilities).
* Pattern recorder, transformation visualizer, time-machine UI, etc.

**Rule of thumb:** anything that benefits from *live modification by users* belongs in XR-Lang; anything that must be **safe, fast, cross-platform, and rarely changed** starts in Rust.

### Concrete bootstrapping path (staged)

**Stage A — Seed (Rust + tiny XR-Lang)**

1. Implement **value model** (numbers, symbols, strings, vectors, maps, AST nodes).
2. Implement **bytecode VM** and a tiny **core evaluator** (enough to run macros and simple functions).
3. Build **AST reader** (s-expr/EDN-like) and **printer**.
4. Implement **journal + snapshot** store; expose patch API.
5. Expose **scene primitives** (entities, transforms, camera) as VM intrinsics.

**Stage B — Homoiconic spine**
6\. Write **macro expander** in XR-Lang (with hygiene).
7\. Port **evaluator** to XR-Lang (metacircular); validate by turning off some Rust builtins.
8\. Add **compiler** XR-Lang → bytecode (simple CPS or ANF pass).
9\. **Selective persistence**: per-slot policies (`preserve`, `volatile`, `sync-to-code`) that compile to patch rules.

**Stage C — Live tooling**
10\. Build **Inspector**, **Object Halo** (Smalltalk/Self-style), **Omnibox REPL** floating in 3D.
11\. Add **Probes**: watch any expression; values render as 2D/3D widgets.
12\. Implement **Time Machine**: snapshot list, journals, branch, travel; step through effects.

**Stage D — Code writes code**
13\. Ship the **source rewriter** and safe **code injection** pipeline (comment fences + provenance).
14\. Pattern recorder → macro generator (with human-in-the-loop review).
15\. Visual shader compiler (XR-Lang front, Rust backend WGSL emit).

**Stage E — AI as peer (capabilities)**
16\. AI calls are **tools** (capabilities): `ai.generate(text, ctx) -> ast`.
17\. All AI-emitted changes arrive as **diff proposals** with reasoning + confidence; user approves or automates under policy.

### Exact merge semantics for your three-layer state

Define it like a DB:

* **Keys**: object id + slot path (e.g., `camera.position`).
* **Values**: typed + format hints.
* **Sources**: `DSL`, `RuntimeOverride`, `Active`.
* **Policy per slot**: `{reset|preserve|sync-to-code|volatile}`, plus **trigger** (`on-stop`, `interval`, `on-gesture-end`).
* **Merge** on reload:

  1. Load DSL → Active.
  2. Apply `RuntimeOverride` patches **that are still valid** (stamp with schema/hash of target to detect drift).
  3. For `sync-to-code` slots, emit **code diffs** (with comments like `/* auto: ... */`) gated by debounce windows and manual-edit detection.

Also add **conflict provenance**:

* `auto vs manual`: stop autosync for that key until human unlocks.
* Hash fences: if surrounding user text changed, queue for review instead of rewrite.

### IDE in 3D: what it should feel like

* **The World *is* the IDE** (Smalltalk image vibe).
* **Halos/Inspectors**: click any object → ring of tools (move, code, state, probes, timeline).
* **Omnibox**: “Do-anything” palette: evaluate code, search objects, jump to definitions.
* **Probe Panels**: drag from any value to spawn a live viz (number → gauge, vector → arrow, list → table, signal → sparkline).
* **Timeline Rail**: scrub time, branch, compare states; causality graph overlays (“this changed that”).
* **Code Surfaces**: code windows are 3D planes; edits are **live**; expansions visualize macro steps on demand.
* **Shader/Behavior Graphs**: node graphs with **round-trip** textual form (select nodes → show generated code with anchors).
* **Policy HUD**: mode badge (Design/Play/Live/Replay), per-slot pins/toggles, autosync indicator.
* **AI Pal**: a spatial agent you can point to objects and say “explain this”, “make it bounce when I tap it”; always proposes diffs, never silently applies unless policy says so.

### Metaprogramming guardrails

* **Two spaces:** *user code* vs *generated sections*. Generated sections are fenced and carry provenance.
* **Semantic diffs** (AST) before textual rewrites; text is a render target.
* **Expand-to-explain**: every macro can produce an *explanation artifact* (why this expansion), shown by the transformation visualizer.
* **No hidden eval:** all run-time evals are logged; “dangerous eval” requires a capability and leaves a breadcrumb in the timeline.

### Testing & reliability (often skipped in live systems)

* **Executable examples** (doctests) on every macro/behavior; visible inline as probes.
* **Record/replay tests** from journals (golden runs).
* **Property tests** for patches: *apply(dsl, patch); serialize; parse; equals?*
* **Fuzz the macro expander** (it’s your most dangerous component).

# Performance strategy

* Start with **bytecode + inline caches**.
* Add **trace recording** for hot paths → optional JIT later.
* Compile shader/compute heavy parts to GPU; XR-Lang orchestrates.
* Keep GC incremental; pace with frame budget (cooperate with the scheduler).

# Practical percentages for bootstrap

* **Phase 1 (MVP usable):** \~45% Rust / 55% XR-Lang.
* **Phase 2 (metacircular + tools):** \~30% Rust / 70% XR-Lang.
* **Phase 3 (mature):** Rust stabilizes around kernel/VM/IO (\~25–30%), *everything else live*.

### A few opinionated picks

* **Syntax:** EDN-ish S-expr with keywords, vectors, maps; readable data literals win for scene DSLs.
* **Modules:** first-class modules with phased macro expansion (compile-time vs run-time).
* **Numbers:** fixnum + bignum; exact rationals later.
* **Concurrency:** actors/mailboxes or effect handlers; integrating with the scheduler is simpler than shared-state.
* **Data binding:** FRP-style signals baked into the value model; probes subscribe under the hood.
* **Serialization:** snapshot = chunked, GC-friendly arena format; journals = CBOR/MessagePack with schema/versioning.

### Risks & mitigations

* **Scope creep:** carve a “thin vertical slice” MVP: *camera preserve + inspector + probes + macro expander + patch journaling*.
* **AI overreach:** keep AI as *proposal engine* with AST diffs + explanations.
* **Source churn:** semantic diffs + provenance fences; never overwrite manual sections.
* **Debug hell:** time machine + probes first; traditional stack traces later.

### Minimal MVP (6–8 features that prove the thesis)

1. EDN/S-expr reader, printer, hygienic macros.
2. Bytecode VM and metacircular evaluator for a subset.
3. Scene with camera/object, hot-reload, **preserve position**.
4. Journal + snapshot; **time scrub** UI.
5. Inspector + **probe** widgets (vector arrow, number gauge).
6. `(meta preserve-runtime | sync-to-code | reset)`.
7. **Source rewriter** that writes camera position back with fenced comments.
8. AI proposal that turns “make cube spin on click” into a diff you approve.

If we build *just that*, we’ve demonstrated Kay/Victor/Smalltalk/Lisp DNA in a modern XR contexts.
