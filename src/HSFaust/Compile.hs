module HSFaust.Compile
  ( compileDsp,
    deleteDspFactory,
    compileDspToCppSource,
  )
where

import           Control.Exception     (bracket)
import qualified Data.Vector.Storable  as V
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (allocaBytes, free)
import           Foreign.Marshal.Array (withArray0)
import           Foreign.Ptr           (Ptr, nullPtr)
import           Foreign.Storable      (peek)
import           HSFaust.Core          (Factory, Signal)
import           HSFaust.FFI

compileDsp ::
  String ->
  IO [Signal] ->
  IO (Either String Factory)
compileDsp appName buildAction =
  bracket createLibContext (\_ -> destroyLibContext) $ \_ -> do
    outputSignals <- buildAction

    withCString appName $ \cAppName ->
      withArray0 nullPtr outputSignals $ \cOutputSignals ->
        withCString "" $ \cEmptyTargetString ->
          let errorBufferSize = 4096
           in allocaBytes errorBufferSize $ \cErrorMsgBuf -> do
                factoryPtr <-
                  c_createCDSPFactoryFromSignals
                    cAppName
                    cOutputSignals
                    0
                    nullPtr
                    cEmptyTargetString
                    cErrorMsgBuf
                    (-1)

                if factoryPtr == nullPtr
                  then do
                    errMsg <- peekCString cErrorMsgBuf
                    return $ Left $ "Faust compilation failed: " ++ errMsg
                  else return $ Right factoryPtr

deleteDspFactory :: Factory -> IO ()
deleteDspFactory = c_deleteCDSPFactory

compileDspToCppSource ::
  String ->
  IO [Signal] ->
  IO (Either String String)
compileDspToCppSource appName buildAction =
  bracket createLibContext (\_ -> destroyLibContext) $ \_ -> do
    outputSignals <- buildAction

    withCString appName $ \cAppName ->
      withArray0 nullPtr outputSignals $ \cOutputSignals ->
        withCString "cpp" $ \cLang -> do
          let errorBufferSize = 4096
          allocaBytes errorBufferSize $ \cErrorMsgBuf -> do
            cSourcePtr <-
              c_createSourceFromSignals
                cAppName
                cOutputSignals
                cLang
                0
                nullPtr
                cErrorMsgBuf

            if cSourcePtr == nullPtr
              then do
                errMsg <- peekCString cErrorMsgBuf
                return $ Left $ "Faust C++ source generation failed: " ++ errMsg
              else do
                cppSource <- peekCString cSourcePtr
                c_freeCMemory cSourcePtr
                return $ Right cppSource
