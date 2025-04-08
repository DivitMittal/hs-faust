# HS-Faust: Haskell Bindings for the Faust Signal API

[![Nix Flake Check](https://github.com/DivitMittal/hs-faust/actions/workflows/flake-check.yaml/badge.svg)](https://github.com/DivitMittal/hs-faust/actions/workflows/flake-check.yaml)

## Overview

`hs-faust` provides Haskell DSL wrapper for the C Signal API of the [Faust](https://faust.grame.fr/). This allows one to define Faust signal processing graphs within Haskell and compile them using the Faust infrastructure.

This project explores using Haskell to construct DSP algorithms that can then be compiled by Faust into various targets (LLVM, C++, etc.).

## Prerequisites

*   **Nix:** The project uses [Nix Flakes](https://nixos.wiki/wiki/Flakes) for managing dependencies and providing a reproducible development environment.
*   **Faust (`libfaust`):** The underlying Faust C library (`libfaust`) is required.

## Current Status

*   **What's Working:**
    *   Bindings and wrappers for few core Faust Signal API functions (constructors, math, delays, UI, recursion).
    *   Compiling simple Haskell-defined DSP graphs to an LLVM DSP Factory using `compileDsp`.
    *   Basic example application (`app/Main.hs`) with a minimal DSP demonstrating compilation.
    *   Reproducible development environment via Nix Flakes.
*   **Known Issues:**
    *   **Runtime Crashes (`SIGILL`):** Using the C++ source generation backend (`compileDspToCppSource`) or potentially more complex signal graphs can cause `SIGILL` crashes. This probably is occuring due to incompatibility between the linked `libfaust` library version & the runtime OS version.

## Usage

Once inside the Nix development shell (`nix develop`), or after having necessary dependencies installed manually, one can:

```bash
cabal run hs-faust-example
```

### Example Code (`app/Main.hs`)

```haskell
import           Foreign.Ptr (nullPtr)
import           HSFaust
import           System.IO   (putStrLn)

-- Minimal DSP definition: A constant 0.5 signal
minimalDsp :: IO [Signal]
minimalDsp = do
    putStrLn "Creating signal: sigReal(0.5)"
    outSignal <- sigReal 0.5
    isSigNil <- isNil outSignal
    if isSigNil
        then putStrLn "No Signal" >> return []
        else return [outSignal]

main :: IO ()
main = do
    let dspName = "MinimalHaskellDSP"
    compileResult <- compileDsp dspName minimalDsp

    case compileResult of
        Left errMsg -> do
            putStrLn $ "Error: reported by Faust: " ++ errMsg

        Right factoryPtr -> do
            if factoryPtr == nullPtr then do
                putStrLn "Error: compileDsp returned a NULL factory pointer!"
            else do
                putStrLn $ "Obtained Factory Pointer: " ++ show factoryPtr
                deleteDspFactory factoryPtr
```
