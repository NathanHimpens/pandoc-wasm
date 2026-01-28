# Pandoc WASM Compilation

## Goal
Compile Pandoc to WebAssembly for converting Markdown to PPTX (and other formats).

## Current State
- ghc-wasm-meta: Fully installed at ~/.ghc-wasm/
- wasmtime: Available at ~/.ghc-wasm/wasmtime/bin/wasmtime (v41.0.0)
- Target: Pandoc 3.8.3 (latest stable on Hackage)
- **STATUS: COMPLETE - pandoc.wasm works!**

## Criteria

### Phase 0: Complete ghc-wasm-meta Installation
- [x] Run bootstrap.sh to install GHC WASM compiler
- [x] Verify wasm32-wasi-ghc --version works (9.12.3.20251231)

### Phase 1: Project Setup and Build
- [x] Create cabal.project with WASM-compatible configuration
- [x] Run wasm32-wasi-cabal update
- [x] Build pandoc-cli with wasm32-wasi-cabal

### Phase 2: Handle Dependency Failures (if needed)
- [x] Address basement WASM/GHC 9.12 compatibility (patched)
- [x] Address digest zlib dependency (disabled pkg-config)
- [x] Address crypton argon2 pthread issue (added ARGON2_NO_THREADS)
- [x] Address xml-conduit Custom build type issue (patched to Simple)
- [x] Address pandoc-cli threaded RTS issue (removed -threaded flag)
- [x] Address network socket stubs for WASI (added socket function stubs)

### Phase 3: Validation
- [x] Create test markdown files (small.md, medium.md, large.md)
- [x] pandoc.wasm binary exists (166MB)
- [x] Successfully convert small.md to PPTX
- [x] Successfully convert medium.md to PPTX
- [x] Successfully convert large.md to PPTX
- [x] Output PPTX files are valid

## Test Command
```bash
# Verify WASM compilation produces working output
source ~/.ghc-wasm/env && wasmtime run --dir . pandoc.wasm -o test.pptx small.md
```

## Notes
- Use flags: `-lua -server` to disable problematic features
- Use `+embed_data_files` to include templates in binary
- wasmtime needs `--dir .` to access host filesystem
- No external processes or network in WASI
- Packages with `build-type: Custom` need to be patched to Simple
- The `-threaded` flag must be removed from executable ghc-options
- Socket functions are stubbed (return ENOSYS) - network features won't work
