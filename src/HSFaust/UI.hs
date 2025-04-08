module HSFaust.UI (
    sigButton,
    sigCheckbox,
    sigVSlider,
    sigHSlider,
    sigNumEntry,
    sigVBargraph,
    sigHBargraph
) where

import           Foreign.C.String
import           HSFaust.FFI
import           HSFaust.Signal   (sigReal)

sigButton :: String -> IO Signal
sigButton label = withCString label c_sigButton

sigCheckbox :: String -> IO Signal
sigCheckbox label = withCString label c_sigCheckbox

sigVSlider :: String
           -> Signal
           -> Signal
           -> Signal
           -> Signal
           -> IO Signal
sigVSlider label initS minS maxS stepS = withCString label $ \cLabel ->
    c_sigVSlider cLabel initS minS maxS stepS

sigHSlider :: String -> Signal -> Signal -> Signal -> Signal -> IO Signal
sigHSlider label initS minS maxS stepS = withCString label $ \cLabel ->
    c_sigHSlider cLabel initS minS maxS stepS

sigNumEntry :: String -> Signal -> Signal -> Signal -> Signal -> IO Signal
sigNumEntry label initS minS maxS stepS = withCString label $ \cLabel ->
    c_sigNumEntry cLabel initS minS maxS stepS

sigVBargraph :: String
             -> Signal
             -> Signal
             -> Signal
             -> IO Signal
sigVBargraph label minS maxS inputS = withCString label $ \cLabel ->
    c_sigVBargraph cLabel minS maxS inputS

sigHBargraph :: String -> Signal -> Signal -> Signal -> IO Signal
sigHBargraph label minS maxS inputS = withCString label $ \cLabel ->
    c_sigHBargraph cLabel minS maxS inputS
