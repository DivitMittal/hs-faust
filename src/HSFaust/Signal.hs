module HSFaust.Signal
  ( -- * Basic Constructors
    sigInt,
    sigReal,
    sigInput,

    -- * Casting
    sigIntCast,
    sigFloatCast,

    -- * Delays
    sigDelay,
    sigDelay1,

    -- * Arithmetic Operators
    sigAdd,
    sigSub,
    sigMul,
    sigDiv,
    sigRem,
    sigLeftShift,
    sigLRightShift,
    sigARightShift,

    -- * Comparison Operators
    sigGT,
    sigLT,
    sigGE,
    sigLE,
    sigEQ,
    sigNE,

    -- * Bitwise Operators
    sigAND,
    sigOR,
    sigXOR,

    -- * Math Functions
    sigAbs,
    sigAcos,
    sigTan,
    sigSqrt,
    sigSin,
    sigRint,
    sigLog,
    sigLog10,
    sigFloor,
    sigExp,
    sigExp10,
    sigCos,
    sigCeil,
    sigAtan,
    sigAsin,
    sigRemainder,
    sigPow,
    sigMin,
    sigMax,
    sigFmod,
    sigAtan2,

    -- * Recursion Primitives
    sigSelf,
    sigRecursion,
    sigSelfN,
    sigRecursionN,

    -- * Soundfile Primitives
    sigSoundfile,
    sigSoundfileLength,
    sigSoundfileRate,
    sigSoundfileBuffer,

    -- * Utilities
    isNil,
    tree2str,

    -- * Sample Rate Helper
    sigSR,
  )
where

import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array (withArray0)
import           Foreign.Ptr           (nullPtr)
import           HSFaust.FFI

-- * Basic Constructors

sigInt :: Int -> IO Signal
sigInt = c_sigInt . fromIntegral

sigReal :: Double -> IO Signal
sigReal = c_sigReal . realToFrac

sigInput :: Int -> IO Signal
sigInput = c_sigInput . fromIntegral

-- * Casting

sigIntCast :: Signal -> IO Signal
sigIntCast = c_sigIntCast

sigFloatCast :: Signal -> IO Signal
sigFloatCast = c_sigFloatCast

-- * Delays

sigDelay :: Signal -> Signal -> IO Signal
sigDelay = c_sigDelay

sigDelay1 :: Signal -> IO Signal
sigDelay1 = c_sigDelay1

-- * Arithmetic Operators

sigAdd :: Signal -> Signal -> IO Signal
sigAdd = c_sigAdd

sigSub :: Signal -> Signal -> IO Signal
sigSub = c_sigSub

sigMul :: Signal -> Signal -> IO Signal
sigMul = c_sigMul

sigDiv :: Signal -> Signal -> IO Signal
sigDiv = c_sigDiv

sigRem :: Signal -> Signal -> IO Signal
sigRem = c_sigRem

sigLeftShift :: Signal -> Signal -> IO Signal
sigLeftShift = c_sigLeftShift

sigLRightShift :: Signal -> Signal -> IO Signal
sigLRightShift = c_sigLRightShift

sigARightShift :: Signal -> Signal -> IO Signal
sigARightShift = c_sigARightShift

-- * Comparison Operators

sigGT :: Signal -> Signal -> IO Signal
sigGT = c_sigGT

sigLT :: Signal -> Signal -> IO Signal
sigLT = c_sigLT

sigGE :: Signal -> Signal -> IO Signal
sigGE = c_sigGE

sigLE :: Signal -> Signal -> IO Signal
sigLE = c_sigLE

sigEQ :: Signal -> Signal -> IO Signal
sigEQ = c_sigEQ

sigNE :: Signal -> Signal -> IO Signal
sigNE = c_sigNE

-- * Bitwise Operators

sigAND :: Signal -> Signal -> IO Signal
sigAND = c_sigAND

sigOR :: Signal -> Signal -> IO Signal
sigOR = c_sigOR

sigXOR :: Signal -> Signal -> IO Signal
sigXOR = c_sigXOR

-- * Math Functions

sigAbs :: Signal -> IO Signal
sigAbs = c_sigAbs

sigAcos :: Signal -> IO Signal
sigAcos = c_sigAcos

sigTan :: Signal -> IO Signal
sigTan = c_sigTan

sigSqrt :: Signal -> IO Signal
sigSqrt = c_sigSqrt

sigSin :: Signal -> IO Signal
sigSin = c_sigSin

sigRint :: Signal -> IO Signal
sigRint = c_sigRint

sigLog :: Signal -> IO Signal
sigLog = c_sigLog

sigLog10 :: Signal -> IO Signal
sigLog10 = c_sigLog10

sigFloor :: Signal -> IO Signal
sigFloor = c_sigFloor

sigExp :: Signal -> IO Signal
sigExp = c_sigExp

sigExp10 :: Signal -> IO Signal
sigExp10 = c_sigExp10

sigCos :: Signal -> IO Signal
sigCos = c_sigCos

sigCeil :: Signal -> IO Signal
sigCeil = c_sigCeil

sigAtan :: Signal -> IO Signal
sigAtan = c_sigAtan

sigAsin :: Signal -> IO Signal
sigAsin = c_sigAsin

sigRemainder :: Signal -> Signal -> IO Signal
sigRemainder = c_sigRemainder

sigPow :: Signal -> Signal -> IO Signal
sigPow = c_sigPow

sigMin :: Signal -> Signal -> IO Signal
sigMin = c_sigMin

sigMax :: Signal -> Signal -> IO Signal
sigMax = c_sigMax

sigFmod :: Signal -> Signal -> IO Signal
sigFmod = c_sigFmod

sigAtan2 :: Signal -> Signal -> IO Signal
sigAtan2 = c_sigAtan2

sigSelf :: IO Signal
sigSelf = c_sigSelf

sigRecursion :: Signal -> IO Signal
sigRecursion = c_sigRecursion

sigSelfN :: Int -> IO Signal
sigSelfN = c_sigSelfN . fromIntegral

sigRecursionN :: [Signal] -> IO [Signal]
sigRecursionN signals = do
  resultPtr <- withArray0 nullPtr signals c_sigRecursionN
  return signals

sigSoundfile :: String -> IO Signal
sigSoundfile label = withCString label c_sigSoundfile

sigSoundfileLength :: Signal -> Signal -> IO Signal
sigSoundfileLength = c_sigSoundfileLength

sigSoundfileRate :: Signal -> Signal -> IO Signal
sigSoundfileRate = c_sigSoundfileRate

sigSoundfileBuffer ::
  Signal ->
  Signal ->
  Signal ->
  Signal ->
  IO Signal
sigSoundfileBuffer = c_sigSoundfileBuffer

isNil :: Signal -> IO Bool
isNil s = (== 1) <$> c_isNil s

tree2str :: Signal -> IO String
tree2str s = c_tree2str s >>= peekCString

-- * Sample Rate Helper

sigSR :: IO Signal
sigSR = do
  srConst <- withCString "fSampleFreq" $ \cName ->
    withCString "<dummy.h>" $ \cFile ->
      c_sigFConst 0 cName cFile
  one <- sigReal 1.0
  maxSr <- sigReal 192000.0
  srClamped <- c_sigMax one =<< sigFloatCast srConst
  c_sigMin maxSr srClamped
