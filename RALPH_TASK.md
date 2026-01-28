 │ │ Pandoc WASM Compilation Plan                                                                                                                      │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Current State Analysis                                                                                                                            │ │
 │ │                                                                                                                                                   │ │
 │ │ • ghc-wasm-meta: Partially installed at ~/.ghc-wasm/ - WASI SDK present but GHC WASM compiler missing                                             │ │
 │ │ • wasmtime: Available at /opt/homebrew/bin/wasmtime                                                                                               │ │
 │ │ • Target: Pandoc 3.8.3 (latest stable on Hackage)                                                                                                 │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Phase 0: Complete ghc-wasm-meta Installation                                                                                                      │ │
 │ │                                                                                                                                                   │ │
 │ │ The GHC WASM compiler is missing. Re-run the bootstrap script:                                                                                    │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.12 sh                                      │ │
 │ │    2 │source ~/.ghc-wasm/env                                                                                                                      │ │
 │ │    3 │wasm32-wasi-ghc --version  # Verify installation                                                                                            │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Phase 1: Attempt Minimal-Modification Build                                                                                                       │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Project Setup                                                                                                                                     │ │
 │ │                                                                                                                                                   │ │
 │ │ Create a new cabal project to build pandoc-cli with WASM-compatible flags:                                                                        │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │pandoc-wasm/                                                                                                                                │ │
 │ │    2 │  cabal.project                                                                                                                             │ │
 │ │    3 │  cabal.project.local                                                                                                                       │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ cabal.project Configuration                                                                                                                       │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │packages: .                                                                                                                                 │ │
 │ │    2 │                                                                                                                                            │ │
 │ │    3 │-- Use Hackage packages                                                                                                                     │ │
 │ │    4 │index-state: 2026-01-28T00:00:00Z                                                                                                           │ │
 │ │    5 │                                                                                                                                            │ │
 │ │    6 │-- Disable problematic features                                                                                                             │ │
 │ │    7 │package pandoc-cli                                                                                                                          │ │
 │ │    8 │  flags: -lua -server                                                                                                                       │ │
 │ │    9 │                                                                                                                                            │ │
 │ │   10 │-- Embed data files for standalone WASM binary                                                                                              │ │
 │ │   11 │package pandoc                                                                                                                              │ │
 │ │   12 │  flags: +embed_data_files                                                                                                                  │ │
 │ │   13 │                                                                                                                                            │ │
 │ │   14 │-- Force constraints for WASM-compatible versions                                                                                           │ │
 │ │   15 │constraints:                                                                                                                                │ │
 │ │   16 │  pandoc == 3.8.3,                                                                                                                          │ │
 │ │   17 │  pandoc-cli == 3.8.3                                                                                                                       │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Build Command                                                                                                                                     │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │source ~/.ghc-wasm/env                                                                                                                      │ │
 │ │    2 │wasm32-wasi-cabal update                                                                                                                    │ │
 │ │    3 │wasm32-wasi-cabal build pandoc-cli --constraint="pandoc-cli -lua -server"                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Phase 2: Handle Dependency Failures (Fallback Strategy)                                                                                           │ │
 │ │                                                                                                                                                   │ │
 │ │ If Phase 1 fails, apply these progressive fixes:                                                                                                  │ │
 │ │                                                                                                                                                   │ │
 │ │ 2a. Network/HTTP Dependencies                                                                                                                     │ │
 │ │                                                                                                                                                   │ │
 │ │ These dependencies are problematic for WASI (no socket support):                                                                                  │ │
 │ │ • network                                                                                                                                         │ │
 │ │ • http-client, http-client-tls                                                                                                                    │ │
 │ │ • crypton-connection, tls                                                                                                                         │ │
 │ │                                                                                                                                                   │ │
 │ │ Strategy: Create a cabal.project with source-repository-package overrides or use --allow-newer with stub implementations.                         │ │
 │ │                                                                                                                                                   │ │
 │ │ 2b. Process/Unix Dependencies                                                                                                                     │ │
 │ │                                                                                                                                                   │ │
 │ │ • process - limited support in WASI                                                                                                               │ │
 │ │ • unix - not available on WASI                                                                                                                    │ │
 │ │                                                                                                                                                   │ │
 │ │ Strategy: Use conditional compilation or patched forks that stub these out.                                                                       │ │
 │ │                                                                                                                                                   │ │
 │ │ 2c. Create Minimal Pandoc Wrapper                                                                                                                 │ │
 │ │                                                                                                                                                   │ │
 │ │ If full pandoc-cli fails, create a minimal wrapper that only uses the pandoc library:                                                             │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │-- Main.hs                                                                                                                                  │ │
 │ │    2 │module Main where                                                                                                                           │ │
 │ │    3 │                                                                                                                                            │ │
 │ │    4 │import Text.Pandoc                                                                                                                          │ │
 │ │    5 │import Text.Pandoc.Class (runIOorExplode)                                                                                                   │ │
 │ │    6 │import qualified Data.Text.IO as T                                                                                                          │ │
 │ │    7 │import System.Environment (getArgs)                                                                                                         │ │
 │ │    8 │                                                                                                                                            │ │
 │ │    9 │main :: IO ()                                                                                                                               │ │
 │ │   10 │main = do                                                                                                                                   │ │
 │ │   11 │  args <- getArgs                                                                                                                           │ │
 │ │   12 │  case args of                                                                                                                              │ │
 │ │   13 │    [inputFormat, outputFormat, inputFile, outputFile] -> do                                                                                │ │
 │ │   14 │      input <- T.readFile inputFile                                                                                                         │ │
 │ │   15 │      result <- runIOorExplode $ do                                                                                                         │ │
 │ │   16 │        doc <- readMarkdown def input                                                                                                       │ │
 │ │   17 │        writePowerpoint def doc                                                                                                             │ │
 │ │   18 │      -- Write output                                                                                                                       │ │
 │ │   19 │      ...                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Phase 3: Build and Optimize                                                                                                                       │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Compile to WASM                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │wasm32-wasi-cabal build pandoc-cli \                                                                                                        │ │
 │ │    2 │  --ghc-options="-O2" \                                                                                                                     │ │
 │ │    3 │  --constraint="pandoc-cli -lua -server"                                                                                                    │ │
 │ │    4 │                                                                                                                                            │ │
 │ │    5 │# Find the binary                                                                                                                           │ │
 │ │    6 │find dist-newstyle -name "pandoc.wasm" -o -name "pandoc"                                                                                    │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Optimize with wasm-opt (optional)                                                                                                                 │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ wasm-opt -O3 pandoc.wasm -o pandoc-optimized.wasm                                                                                                 │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Phase 4: Validation                                                                                                                               │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Create Test Files                                                                                                                                 │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │# small.md - Simple document                                                                                                                │ │
 │ │    2 │echo "# Hello World\n\nThis is a test." > small.md                                                                                          │ │
 │ │    3 │                                                                                                                                            │ │
 │ │    4 │# medium.md - Multiple slides                                                                                                               │ │
 │ │    5 │cat > medium.md << 'EOF'                                                                                                                    │ │
 │ │    6 │---                                                                                                                                         │ │
 │ │    7 │title: Test Presentation                                                                                                                    │ │
 │ │    8 │author: Test Author                                                                                                                         │ │
 │ │    9 │---                                                                                                                                         │ │
 │ │   10 │                                                                                                                                            │ │
 │ │   11 │# Slide 1                                                                                                                                   │ │
 │ │   12 │Content for slide 1                                                                                                                         │ │
 │ │   13 │                                                                                                                                            │ │
 │ │   14 │# Slide 2                                                                                                                                   │ │
 │ │   15 │- Bullet point 1                                                                                                                            │ │
 │ │   16 │- Bullet point 2                                                                                                                            │ │
 │ │   17 │                                                                                                                                            │ │
 │ │   18 │# Slide 3                                                                                                                                   │ │
 │ │   19 │More content here                                                                                                                           │ │
 │ │   20 │EOF                                                                                                                                         │ │
 │ │   21 │                                                                                                                                            │ │
 │ │   22 │# large.md - More complex document                                                                                                          │ │
 │ │   23 │# (Generate with multiple sections, tables, images references)                                                                              │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Run Validation Tests                                                                                                                              │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │# Test PPTX output                                                                                                                          │ │
 │ │    2 │wasmtime run --dir . pandoc.wasm -- -f markdown -t pptx -o small.pptx small.md                                                              │ │
 │ │    3 │wasmtime run --dir . pandoc.wasm -- -f markdown -t pptx -o medium.pptx medium.md                                                            │ │
 │ │    4 │wasmtime run --dir . pandoc.wasm -- -f markdown -t pptx -o large.pptx large.md                                                              │ │
 │ │    5 │                                                                                                                                            │ │
 │ │    6 │# Verify outputs exist and are valid                                                                                                        │ │
 │ │    7 │file small.pptx medium.pptx large.pptx                                                                                                      │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ PDF via Typst (if LaTeX unavailable)                                                                                                              │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │    1 │# Typst is natively supported by Pandoc                                                                                                     │ │
 │ │    2 │wasmtime run --dir . pandoc.wasm -- -f markdown -t typst -o output.typ input.md                                                             │ │
 │ │                                                                                                                                                   │ │
 │ │ Note: Direct PDF generation requires external tools (pdflatex, typst binary) which won't work in WASI. The Typst format can be converted to PDF   │ │
 │ │  externally.                                                                                                                                      │ │
 │ │                                                                                                                                                   │ │
 │ │ Key Technical Considerations                                                                                                                      │ │
 │ │                                                                                                                                                   │ │
 │ │ 1. Memory: WASM has 4GB linear memory limit - large documents may need memory tuning                                                              │ │
 │ │ 2. Data Files: Use +embed_data_files flag to include templates in binary                                                                          │ │
 │ │ 3. File System: wasmtime needs --dir . to access host filesystem                                                                                  │ │
 │ │ 4. No External Processes: PDF generation via LaTeX won't work (no process spawning)                                                               │ │
 │ │ 5. No Network: HTTP fetching of resources disabled                                                                                                │ │
 │ │                                                                                                                                                   │ │
 │ │                                                                                                                                                   │ │
 │ │ Success Criteria                                                                                                                                  │ │
 │ │                                                                                                                                                   │ │
 │ │ • pandoc.wasm binary generated (expect 50-100MB with embedded data)                                                                               │ │
 │ │ • Successfully converts small.md, medium.md, large.md to PPTX via wasmtime                                                                        │ │
 │ │ • Output PPTX files are valid and openable                                                                                                        │ │
 │ │                                                                        
