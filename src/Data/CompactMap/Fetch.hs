{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module Data.CompactMap.Fetch
  ( getElement
  , extractElement
  , extractElementBS
  , extractElementInt
  ) where

import           Foreign
import           GHC.Ptr

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Unsafe as B

import           Data.Binary

sizeOfInt :: Int
sizeOfInt = sizeOf (0::Int)

{-# INLINE getElement #-}
getElement :: (Binary a) => Ptr () -> IO a
getElement ptr
    = do size <- peek (castPtr ptr) :: IO Int
         extractElement (ptr `plusPtr` (sizeOfInt * 1)) size

{-# INLINE [2] extractElement #-}
extractElement :: Binary a => Ptr () -> Int -> IO a
extractElement !ptr !size
    = do !bs <- B.unsafePackCStringLen (castPtr ptr, size)
         return $! decode (LBS.fromChunks [bs])

{-# RULES "extractElement/Int" extractElement = extractElementInt #-}
extractElementInt :: Ptr () -> Int -> IO Int
extractElementInt !ptr !size
    = do !bs <- B.unsafePackCStringLen (castPtr ptr, size)
         let !x = decode (LBS.fromChunks [bs]) :: Int
         b1 <- fmap fromIntegral (peek (ptr `plusPtr` (0)) :: IO Word8)
         b2 <- fmap fromIntegral (peek (ptr `plusPtr` (1)) :: IO Word8)
         b3 <- fmap fromIntegral (peek (ptr `plusPtr` (2)) :: IO Word8)
         b4 <- fmap fromIntegral (peek (ptr `plusPtr` (3)) :: IO Word8)
         b5 <- fmap fromIntegral (peek (ptr `plusPtr` (4)) :: IO Word8)
         b6 <- fmap fromIntegral (peek (ptr `plusPtr` (5)) :: IO Word8)
         b7 <- fmap fromIntegral (peek (ptr `plusPtr` (6)) :: IO Word8)
         b8 <- fmap fromIntegral (peek (ptr `plusPtr` (7)) :: IO Word8)
         return $ b1 `shiftL` 56 .|.
                  b2 `shiftL` 48 .|.
                  b3 `shiftL` 40 .|.
                  b4 `shiftL` 32 .|.
                  b5 `shiftL` 24 .|.
                  b6 `shiftL` 16 .|.
                  b7 `shiftL` 8 .|.
                  b8 `shiftL` 0

{-# RULES "extractElement/ByteString" extractElement = extractElementBS #-}
extractElementBS :: Ptr () -> Int -> IO B.ByteString
extractElementBS ptr !size
    = let n = sizeOf (0::Int)
          !(Ptr addr#) = ptr `plusPtr` n
      in B.unsafePackAddressLen (size-n) addr#
