{-# LANGUAGE BangPatterns #-}
module Data.CompactMap.Buffer
  ( newBuffer
  , withBytes
  , touchBuffer
  ) where



import Data.CompactMap.Types

import Foreign            (Ptr, plusPtr, castPtr)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr, castForeignPtr)
import Data.IORef
import GHC.ForeignPtr     (mallocPlainForeignPtrBytes)

newBuffer :: Int -> IO Buffer
newBuffer initSize
    = do fptr   <- newIORef =<< mallocPlainForeignPtrBytes initSize
         old    <- newIORef []
         posRef <- newFastMutInt 0
         size   <- newFastMutInt initSize
         return $ Buffer{ bufferData = fptr
                        , bufferOld  = old
                        , bufferPos  = posRef
                        , bufferSize = size }

withBytes :: Buffer -> Int -> (Ptr a -> IO b) -> IO b
withBytes !Buffer{bufferPos=bufferPos,bufferData=bufferData,bufferSize=bufferSize,bufferOld=bufferOld} !bytesNeeded fn
    = do !currentPos <- readFastMutInt bufferPos
         !currentSize <- readFastMutInt bufferSize
         !oldPtr <- readIORef bufferData
         if currentSize >= currentPos + bytesNeeded
            then do writeFastMutInt bufferPos (currentPos+bytesNeeded)
                    withForeignPtr (castForeignPtr oldPtr) $ \ptr -> fn $! (ptr `plusPtr` currentPos)
            else do let minSize = max bytesNeeded currentSize
                        newSize = minSize + minSize `div` 4 -- Add 25% to the buffer.
                    fptr <- mallocPlainForeignPtrBytes newSize
                    writeIORef bufferData fptr
                    modifyIORef bufferOld (oldPtr:)
                    writeFastMutInt bufferPos bytesNeeded
                    writeFastMutInt bufferSize newSize -- aligned
                    withForeignPtr fptr $ fn . castPtr

touchBuffer :: Buffer -> IO ()
touchBuffer buffer
    = do touchForeignPtr =<< readIORef (bufferData buffer)
         ls <- readIORef (bufferOld buffer)
         mapM_ touchForeignPtr ls
