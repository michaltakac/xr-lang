# Repository Guidelines

## Project Structure & Module Organization
- Rust workspace in `Cargo.toml` with crates: `dsl/`, `ir/`, `jit/`, `vm/`, `gpu/`, `xr/`, `ai/`, `cv/`, `testing/`, and app hosts in `app-hosts/{desktop,quest,web}`.
- Examples in `examples/*.xrdsl`; docs in `docs/`; CI and helper scripts in root (`Makefile`, `build.sh`, `test_runner.sh`).
- Integration tests live in each crate’s `tests/` directory; top-level `tests/` contains cross‑crate tests.

## Build, Test, and Development Commands
- Build all crates: `make build` (uses `cargo build --workspace --all-features`).
- Run all tests: `make test`; focused suites: `make test-dsl`, `make test-vm`, `make test-gpu`, or `make test-integration`.
- Lint and format: `make fmt` (rustfmt), `make lint` (clippy with `-D warnings`), `make check`.
- Run desktop host: `cargo run -p desktop -- examples/spinning_cubes.xrdsl`.
- Platform builds: `./build.sh --platform {desktop|quest|web} [--release]`.

## Coding Style & Naming Conventions
- Rust 2021 edition with `rustfmt`; 4‑space indentation; wrap lines reasonably (~100 cols).
- Naming: crates and modules `snake_case`; types and traits `CamelCase`; functions and vars `snake_case`; constants `SCREAMING_SNAKE_CASE`.
- Prefer small, focused modules; avoid unrelated refactors in the same PR.
- Run `make fmt` and `make lint` before pushing; fix all clippy warnings.

## Testing Guidelines
- Use Rust’s built‑in test harness; async tests with `#[tokio::test]`.
- Place integration tests under `crate/tests/`; name tests descriptively (e.g., `test_parse_nested_lists`).
- Quick loops: `make quick-test`; watch mode: `make watch`.
- Coverage (optional locally): `make coverage` (tarpaulin required). See `TESTING.md` for details and categories.

## Commit & Pull Request Guidelines
- Commit style: Conventional Commits (`feat:`, `fix:`, `refactor:`, `chore:`, `wip:`). Keep messages imperative and scoped (e.g., `feat(vm): add heap compaction`).
- Before opening a PR: run `make ci`; include a clear description, linked issues, and screenshots/GIFs for visual or DX changes.
- PRs should be small and reviewable; document breaking changes and migration notes.

## Security & Configuration Tips
- Quest/Android builds require NDK: set `ANDROID_HOME` and `ANDROID_NDK_HOME`; install `cargo-apk`.
- Web builds require `wasm-pack` and `wasm32-unknown-unknown` target.
- GPU tests can be flaky headless; rely on CI allowances and see `TESTING.md` troubleshooting.

