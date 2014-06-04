{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.CompactMap.Types where

import           Control.Monad
import           Foreign
import           Foreign.Storable

import           Data.IORef
import           GHC.Exts
import           GHC.IO           hiding (Buffer)

data Buffer = Buffer
    { bufferData :: {-# UNPACK #-} !(IORef (ForeignPtr ()))
    , bufferOld  :: {-# UNPACK #-} !(IORef [ForeignPtr ()])
    , bufferPos  :: {-# UNPACK #-} !FastMutInt
    , bufferSize :: {-# UNPACK #-} !FastMutInt
    }


-- Strict, unboxed IORef
data FastMutInt = FastMutInt (MutableByteArray# RealWorld)
newFastMutInt (I# i) = IO $ \s -> case newByteArray# size s of
                                    (# s, arr #) -> case writeIntArray# arr 0# i s of
                                                      s -> (# s, FastMutInt arr #)
    where !(I# size) = sizeOf (0::Int)
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }


data KeyCursor
data DataCursor

data Index = Index { indexStart  :: {-# UNPACK #-} !(Ptr IndexItem)
                   , indexBuffer :: {-# UNPACK #-} !Buffer }

data IndexItem = IndexItem {-# UNPACK #-} !(Ptr IndexItem)
                           {-# UNPACK #-} !(Ptr ())
                           {-# UNPACK #-} !(Ptr KeyCursor)
                           {-# UNPACK #-} !(Ptr IndexItem)
                           {-# UNPACK #-} !(Ptr IndexItem) -- Top, size, elem idx, left, right


type IdxInt = Ptr IndexItem
{-# INLINE extractField #-}
-- Get field 'f' out of the n'th IndexItem.
extractField :: Int -> (Ptr IndexItem) -> IO (Ptr IndexItem)
extractField !f !ptr = do v <- peekByteOff ptr ((sizeOf (undefined::IdxInt) * f))
                          return (v::IdxInt)

{-# INLINE putField #-}
-- Put field 'f' in the n'th IndexItem
putField :: Int -> (Ptr IndexItem) -> Ptr IndexItem -> IO ()
putField !f !ptr !v = pokeByteOff ptr ((sizeOf (undefined::IdxInt) * f)) (v :: IdxInt)


instance Storable IndexItem where
    sizeOf _ = sizeOf (undefined :: IdxInt) * 5
    alignment _ = alignment (undefined :: IdxInt)
    {-# INLINE peek #-}
    peek ptr = let ptr' = castPtr ptr
                   get n = (peekElemOff ptr' n :: IO (Ptr a))
               in liftM5 IndexItem (get 0) (get 1) (get 2) (get 3) (get 4)
    {-# INLINE poke #-}
    poke ptr' (IndexItem a b c d e)
        = let ptr = castPtr ptr'
              put n v = pokeElemOff ptr n v
          in put 0 a >> put 1 b >> put 2 c >> put 3 d >> put 4 e


