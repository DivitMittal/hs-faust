import           Foreign.Ptr (nullPtr)
import           HSFaust
import           System.IO   (putStrLn)

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
      if factoryPtr == nullPtr
        then do
          putStrLn "Error: compileDsp returned a NULL factory pointer!"
        else do
          putStrLn $ "Obtained Factory Pointer: " ++ show factoryPtr
          deleteDspFactory factoryPtr
