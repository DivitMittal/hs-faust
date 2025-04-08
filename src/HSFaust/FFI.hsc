{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HSFaust.FFI where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

#include <faust/dsp/libfaust-c.h>
#include <faust/dsp/libfaust-signal-c.h>
#include <faust/dsp/llvm-dsp-c.h>

data CTree
type Signal = Ptr CTree

data LLVMDspFactory
type Factory = Ptr LLVMDspFactory

foreign import ccall unsafe "createLibContext"
    createLibContext :: IO ()

foreign import ccall unsafe "destroyLibContext"
    destroyLibContext :: IO ()

foreign import ccall unsafe "CsigInt"
    c_sigInt :: CInt -> IO Signal

foreign import ccall unsafe "CsigReal"
    c_sigReal :: CDouble -> IO Signal

foreign import ccall unsafe "CsigInput"
    c_sigInput :: CInt -> IO Signal

foreign import ccall unsafe "CsigDelay"
    c_sigDelay :: Signal -> Signal -> IO Signal

foreign import ccall unsafe "CsigDelay1"
    c_sigDelay1 :: Signal -> IO Signal

foreign import ccall unsafe "CsigIntCast"
    c_sigIntCast :: Signal -> IO Signal

foreign import ccall unsafe "CsigFloatCast"
    c_sigFloatCast :: Signal -> IO Signal

foreign import ccall unsafe "CsigAdd"
    c_sigAdd :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigSub"
    c_sigSub :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigMul"
    c_sigMul :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigDiv"
    c_sigDiv :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigRem"
    c_sigRem :: Signal -> Signal -> IO Signal

foreign import ccall unsafe "CsigLeftShift"
    c_sigLeftShift :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigLRightShift"
    c_sigLRightShift :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigARightShift"
    c_sigARightShift :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigAND"
    c_sigAND :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigOR"
    c_sigOR :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigXOR"
    c_sigXOR :: Signal -> Signal -> IO Signal

foreign import ccall unsafe "CsigGT"
    c_sigGT :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigLT"
    c_sigLT :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigGE"
    c_sigGE :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigLE"
    c_sigLE :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigEQ"
    c_sigEQ :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigNE"
    c_sigNE :: Signal -> Signal -> IO Signal

foreign import ccall unsafe "CsigAbs"
    c_sigAbs :: Signal -> IO Signal
foreign import ccall unsafe "CsigAcos"
    c_sigAcos :: Signal -> IO Signal
foreign import ccall unsafe "CsigAsin"
    c_sigAsin :: Signal -> IO Signal
foreign import ccall unsafe "CsigAtan"
    c_sigAtan :: Signal -> IO Signal
foreign import ccall unsafe "CsigAtan2"
    c_sigAtan2 :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigCeil"
    c_sigCeil :: Signal -> IO Signal
foreign import ccall unsafe "CsigCos"
    c_sigCos :: Signal -> IO Signal
foreign import ccall unsafe "CsigExp"
    c_sigExp :: Signal -> IO Signal
foreign import ccall unsafe "CsigExp10"
    c_sigExp10 :: Signal -> IO Signal
foreign import ccall unsafe "CsigFloor"
    c_sigFloor :: Signal -> IO Signal
foreign import ccall unsafe "CsigFmod"
    c_sigFmod :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigLog"
    c_sigLog :: Signal -> IO Signal
foreign import ccall unsafe "CsigLog10"
    c_sigLog10 :: Signal -> IO Signal
foreign import ccall unsafe "CsigMax"
    c_sigMax :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigMin"
    c_sigMin :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigPow"
    c_sigPow :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigRemainder"
    c_sigRemainder :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigRint"
    c_sigRint :: Signal -> IO Signal
foreign import ccall unsafe "CsigSin"
    c_sigSin :: Signal -> IO Signal
foreign import ccall unsafe "CsigSqrt"
    c_sigSqrt :: Signal -> IO Signal
foreign import ccall unsafe "CsigTan"
    c_sigTan :: Signal -> IO Signal

foreign import ccall unsafe "CsigSelf"
    c_sigSelf :: IO Signal
foreign import ccall unsafe "CsigRecursion"
    c_sigRecursion :: Signal -> IO Signal
foreign import ccall unsafe "CsigSelfN"
    c_sigSelfN :: CInt -> IO Signal
foreign import ccall unsafe "CsigRecursionN"
    c_sigRecursionN :: Ptr Signal -> IO (Ptr Signal)

foreign import ccall unsafe "CsigButton"
    c_sigButton :: CString -> IO Signal
foreign import ccall unsafe "CsigCheckbox"
    c_sigCheckbox :: CString -> IO Signal
foreign import ccall unsafe "CsigVSlider"
    c_sigVSlider :: CString -> Signal -> Signal -> Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigHSlider"
    c_sigHSlider :: CString -> Signal -> Signal -> Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigNumEntry"
    c_sigNumEntry :: CString -> Signal -> Signal -> Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigVBargraph"
    c_sigVBargraph :: CString -> Signal -> Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigHBargraph"
    c_sigHBargraph :: CString -> Signal -> Signal -> Signal -> IO Signal

foreign import ccall unsafe "CsigSoundfile"
    c_sigSoundfile :: CString -> IO Signal
foreign import ccall unsafe "CsigSoundfileLength"
    c_sigSoundfileLength :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigSoundfileRate"
    c_sigSoundfileRate :: Signal -> Signal -> IO Signal
foreign import ccall unsafe "CsigSoundfileBuffer"
    c_sigSoundfileBuffer :: Signal -> Signal -> Signal -> Signal -> IO Signal


foreign import ccall unsafe "createCDSPFactoryFromSignals"
    c_createCDSPFactoryFromSignals :: CString
                                   -> Ptr Signal
                                   -> CInt
                                                                      -> Ptr CString
                                   -> CString
                                   -> CString
                                   -> CInt
                                   -> IO Factory

foreign import ccall unsafe "deleteCDSPFactory"
    c_deleteCDSPFactory :: Factory -> IO ()

foreign import ccall unsafe "CisNil"
    c_isNil :: Signal -> IO CBool

foreign import ccall unsafe "Ctree2str"
    c_tree2str :: Signal -> IO CString

foreign import ccall unsafe "CsigFConst"
    c_sigFConst :: CInt -> CString -> CString -> IO Signal

foreign import ccall unsafe "freeCMemory"
    c_freeCMemory :: Ptr a -> IO ()
foreign import ccall unsafe "generateCSHA1"
    c_generateCSHA1 :: CString -> CString -> IO ()
foreign import ccall unsafe "CcreateSourceFromSignals"
    c_createSourceFromSignals :: CString
                              -> Ptr Signal
                              -> CString
                              -> CInt
                              -> Ptr CString
                              -> CString
                              -> IO CString