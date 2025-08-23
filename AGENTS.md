## Project Overview

**hs-faust** is a Haskell DSL wrapper for the Faust signal processing compiler's C Signal API. It allows defining DSP (Digital Signal Processing) algorithms in Haskell and compiling them using Faust's infrastructure to generate LLVM or C++ code.

## Build System & Development Commands

### Nix Flakes (Primary Build System)

This project uses **Nix Flakes** with a modular `flake-parts` architecture. All flake configuration lives in the `/flake` directory.

```bash
# Enter development environment with all dependencies
nix develop

# Or use direnv for automatic environment loading
direnv allow

# Validate entire flake (pre-commit checks, build, etc.)
nix flake check --impure --all-systems --no-build
```

### Cabal Commands

```bash
# Inside nix develop shell:
cabal build                      # Build entire project
cabal build lib:hs-faust         # Build library only
cabal build exe:hs-faust-example # Build example executable
cabal run hs-faust-example       # Run example application
```

### Code Formatting

The project uses `treefmt-nix` for unified formatting:

```bash
# Format all code (Haskell, Nix, C/C++)
treefmt

# Individual formatters:
ormolu --mode inplace src/HSFaust/*.hs app/Main.hs  # Haskell
alejandra flake.nix flake/                          # Nix
clang-format -i src/HSFaust/FFI.hsc                 # C/C++
cabal-fmt hs-faust.cabal                            # Cabal
```

### Static Analysis

```bash
statix check flake/     # Nix linting
deadnix --check flake/  # Dead code detection
```

## Architecture & Code Structure

### Core Components

1. **FFI Layer** (`src/HSFaust/FFI.hsc`)
   - Uses `hsc2hs` preprocessor for C FFI bindings to `libfaust`
   - Defines opaque types: `Signal` (Ptr CTree) and `Factory` (Ptr LLVMDspFactory)
   - Contains 80+ foreign imports for libfaust C functions

2. **Signal Operations** (`src/HSFaust/Signal.hs`)
   - **Constructors**: `sigInt`, `sigReal`, `sigInput` - create primitive signals
   - **Operators**: Arithmetic (`sigAdd`, `sigMul`), comparison (`sigGT`, `sigEQ`), bitwise (`sigAND`, `sigOR`)
   - **Math Functions**: `sigSin`, `sigCos`, `sigSqrt`, `sigExp`, etc.
   - **Delays & Recursion**: `sigDelay`, `sigDelay1`, `sigRecursion`, `sigSelfN` (feedback loops)
   - **Soundfile Support**: `sigSoundfile`, `sigSoundfileLength`, etc.

3. **UI Controls** (`src/HSFaust/UI.hs`)
   - Interactive widgets: `sigButton`, `sigCheckbox`, `sigVSlider`, `sigHSlider`, `sigNumEntry`
   - Output visualization: `sigVBargraph`, `sigHBargraph`

4. **Compilation Pipeline** (`src/HSFaust/Compile.hs`)
   - **`compileDsp`**: Main entry point - takes app name and IO action producing signal graph, returns `Either String Factory`
   - **`compileDspToCppSource`**: Generates C++ source (⚠️ **Known to cause SIGILL crashes** - see below)
   - **`deleteDspFactory`**: Resource cleanup for compiled factories

### Type System

All signal operations return `IO Signal` or `IO Factory` - monadic for FFI safety:
- **Signal**: Opaque pointer to Faust's internal signal tree (`Ptr CTree`)
- **Factory**: Opaque pointer to LLVM DSP factory (`Ptr LLVMDspFactory`)

## Known Issues & Limitations

### ⚠️ Runtime SIGILL Crashes

Using `compileDspToCppSource` or complex signal graphs can cause segmentation faults.

- **Root Cause**: Likely libfaust version incompatibility with runtime OS
- **Impact**: C++ code generation backend is unreliable
- **Workaround**: Use LLVM backend (`compileDsp`) for stable operation

## Dependencies

### External Requirements
- **libfaust**: Must be installed at `/usr/local/lib` and `/usr/local/include`
  - Configured in `hs-faust.cabal` via `extra-libraries: faust` and `include-dirs`

### Haskell Dependencies (from cabal)
- `base >= 4.7 && < 5`
- `bytestring`
- `vector`

### Build Tools
- `hsc2hs`: For FFI binding generation
- Provided by Nix: GHC, Cabal, Ormolu, HLS, nixd, clang-tools

## Flake Architecture

The flake uses a modular structure via `flake-parts`:

```
flake/
├── default.nix      # Main module loader
├── devshells.nix    # Development environment setup
├── checks.nix       # Pre-commit hooks (trailing whitespace, large files, merge conflicts, private keys)
├── formatters.nix   # treefmt-nix configuration (Ormolu, Alejandra, clang-format, cabal-fmt)
└── actions/         # GitHub Actions CI/CD workflow definitions
```

**Flake Inputs**:
- `nixpkgs` (nixos/nixpkgs unstable)
- `flake-parts` - Modular flake configuration
- `devshell` - Development shell management
- `treefmt-nix` - Unified formatting
- `git-hooks` - Pre-commit integration
- `actions-nix` - CI/CD workflow generation
- `OS-nixCfg` - Custom configuration utilities

## CI/CD Workflows

Located in `.github/workflows/`:

1. **flake-check.yml** - Runs on push/PR to validate entire flake
2. **flake-lock-update.yml** - Weekly automated dependency updates (Sundays at midnight)

## Example: Creating a DSP

```haskell
import HSFaust
import Foreign.Ptr (nullPtr)

-- Define signal graph
myDsp :: IO [Signal]
myDsp = do
  sig <- sigReal 0.5  -- Constant signal
  isNil <- isNil sig
  if isNil then return [] else return [sig]

-- Compile to LLVM
main :: IO ()
main = do
  result <- compileDsp "MyDSP" myDsp
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right factory -> do
      putStrLn $ "Success: " ++ show factory
      deleteDspFactory factory
```

## GHC Compiler Options

The project uses strict warnings (configured in `hs-faust.cabal`):
- `-Wall`, `-Wcompat`, `-Wmissing-export-lists`, `-Wincomplete-uni-patterns`
- `-Wincomplete-record-updates`, `-Wredundant-constraints`, `-Wno-name-shadowing`