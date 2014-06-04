{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.CompactMap.Index where

import           Control.Monad
import           Data.Maybe
import           Foreign                  hiding (rotateL, rotateR)
import           Foreign.Storable
import           System.IO.Unsafe

--import Data.Array.IArray
import           Data.Array.IO
import           Data.Array.Unboxed

import           Data.Binary

import qualified Data.ByteString          as Strict
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy     as Lazy
import qualified Data.ByteString.Unsafe   as Strict

import           Data.CompactMap.Buffer
import           Data.CompactMap.Fetch
import           Data.CompactMap.Types

import           Data.Array.Unsafe
import           GHC.Exts
import           Prelude                  hiding (Either (..))

type Tag = Int


-- ints are native GHC ints.
{- KeyCursor
void *dataPointer;
int keyLen;
void *key;
-}
{- DataCursor
void *next;
int tag;
word8 isJust;
int dataLen;
void *data;
-}

peekKeyCursorData :: Ptr KeyCursor -> IO (Ptr DataCursor)
peekKeyCursorData ptr
    = peek (castPtr ptr)

peekKeyCursorKey :: Ptr KeyCursor -> IO Strict.ByteString
peekKeyCursorKey ptr = do len <- peek (ptr `plusPtr` ptrSize)
                          Strict.unsafePackCStringLen (ptr `plusPtr` (ptrSize+intSize), len)

pokeKeyCursorData :: Ptr KeyCursor -> Ptr DataCursor -> IO ()
pokeKeyCursorData ptr dataPtr
    = poke (castPtr ptr) dataPtr

newKeyCursor :: Buffer -> Lazy.ByteString -> IO (Ptr KeyCursor)
newKeyCursor buffer keyE
    = withBytes buffer (intSize*2 + keyLen) $ \keyPtr ->
      do poke (castPtr keyPtr) nullPtr
         putByteString (keyPtr `plusPtr` intSize) keyE keyLen
         return keyPtr
    where keyLen = fromIntegral $ Lazy.length keyE

newBinaryKeyCursor :: (Binary a) => Buffer -> a -> IO (Ptr KeyCursor)
newBinaryKeyCursor !buffer !key
    = newKeyCursor buffer (encode key)

pushNewDataCursor :: Ptr KeyCursor -> Ptr DataCursor -> IO ()
pushNewDataCursor keyCursor dataCursor
    = do oldData <- peekKeyCursorData keyCursor
         pokeDataCursorNext dataCursor oldData
         pokeKeyCursorData keyCursor dataCursor

peekDataCursorNext :: Ptr DataCursor -> IO (Ptr DataCursor)
peekDataCursorNext ptr = peek (castPtr ptr)

peekDataCursorTag :: Ptr DataCursor -> IO Int
peekDataCursorTag ptr = peek (ptr `plusPtr` ptrSize)

peekDataCursorData :: Ptr DataCursor -> IO (Maybe Strict.ByteString)
peekDataCursorData ptr
    = do isJ <- peek (ptr `plusPtr` (ptrSize+intSize))
         case isJ == (1::Word8) of
           False -> do return Nothing
           True  -> do len <- peek (ptr `plusPtr` (ptrSize+intSize+1))
                       bs <- Strict.unsafePackCStringLen (ptr `plusPtr` (intSize+intSize+1+intSize),len)
                       return (Just bs)

pokeDataCursorNext :: Ptr DataCursor -> Ptr DataCursor -> IO ()
pokeDataCursorNext ptr next
    = poke (castPtr ptr) next

newDataCursor :: Buffer -> Tag -> Maybe Lazy.ByteString -> IO (Ptr DataCursor)
newDataCursor !buffer !tag !mbString
    = do let !bsLen = fromIntegral $ maybe 0 Lazy.length mbString
             !ext = if isJust mbString then intSize else 0
         withBytes buffer (ptrSize+intSize+1+bsLen+ext) $ \ !ptr ->
           do poke (castPtr ptr) (nullPtr :: Ptr DataCursor)
              poke (ptr `plusPtr` ptrSize) tag
              case mbString of
                Nothing -> do poke (ptr `plusPtr` (ptrSize+intSize)) (0::Word8)
                Just bs -> do poke (ptr `plusPtr` (ptrSize+intSize)) (1::Word8)
                              putByteString (ptr `plusPtr` (ptrSize+intSize+1)) bs bsLen
              return ptr


intToPtr i = nullPtr `plusPtr` i
ptrToInt (Ptr addr#) = I# (addr2Int# addr#)

extractTop = extractField 0
extractSize = fmap ptrToInt . extractField 1
extractElemIdx = fmap castPtr . extractField 2
extractLeft = extractField 3
extractRight = extractField 4

putTop ptr val = if ptr == nullPtr then return () else putField 0 ptr val
putSize p s = putField 1 p (intToPtr s)
--putElemIdx p e = putField 2 p (castPtr e)
putLeft = putField 3
putRight :: Ptr IndexItem -> Ptr IndexItem -> IO ()
putRight = putField 4

--getElement set e = return $ set IntMap.! e
{-
ppHex n = "0x" ++ (drop (length hex) zeroes) ++ hex
    where hex = showHex n ""
          zeroes = replicate ((sizeOf nullPtr) * 2) '0'
-}
data Direction = Left | Right | Stop

{-# INLINE walkTree #-}
walkTree start move
    = let loop n = do keyCursor <- extractElemIdx n
                      dir <- move keyCursor
                      case dir of
                        Left  -> extractLeft n >>= \left ->
                                 if left == nullPtr
                                 then return (Left, n) else loop left
                        Right -> extractRight n >>= \right ->
                                 if right == nullPtr
                                 then return (Right, n) else loop right
                        Stop  -> return (Stop, n)
      in loop start


{-# INLINE lookupNearest #-}
lookupNearest :: (Ord a, Binary a) => Ptr IndexItem
              -> a -> IO (Direction, Ptr IndexItem)
lookupNearest start e
    = walkTree start $ \keyCursor ->
      do idxElem <- getElement (keyCursor `plusPtr` intSize)
         case compare e idxElem of
           LT -> return Left
           GT -> return Right
           EQ -> return Stop

{-# INLINE lookupLargest #-}
lookupLargest :: Ptr IndexItem
              -> IO (Direction, Ptr IndexItem)
lookupLargest start
    = walkTree start $ \_ -> return Right


putByteString :: Ptr () -> Lazy.ByteString -> Int -> IO ()
putByteString dst lbs len
    = do poke (castPtr dst) len
         let loop !ptr [] = return ()
             loop !ptr (chunk:cs) = do Strict.unsafeUseAsCString chunk $ \cstr ->
                                         copyArray ptr cstr (Strict.length chunk)
                                       loop (ptr `plusPtr` Strict.length chunk) cs
         loop (dst `plusPtr` intSize) (Lazy.toChunks lbs)


intSize :: Int
intSize = sizeOf (undefined::Int)

ptrSize :: Int
ptrSize = sizeOf (undefined::Ptr ())

insert :: (Ord k, Binary k, Binary a) => Index -> k -> Tag -> Maybe a -> IO [(Tag,Maybe Strict.ByteString)]
insert idx key tag mbVal
    = insertBS idx key tag (fmap encode mbVal)

{-
insertWith :: (Ord k, Binary k, Binary a) => Index -> k -> Tag -> (Ptr DataCursor -> IO (Maybe a)) -> IO [(Tag,Maybe Strict.ByteString)]
insertWith idx key tag genVal
    = insertWithBS idx key tag (\dataCursor -> do val <- genVal dataCursor; return $ fmap encode val)
-}
{- SPECIALISE insertBS :: Index -> Strict.ByteString -> Tag -> Maybe Lazy.ByteString -> IO [(Tag,Maybe Strict.ByteString)] -}
insertBS :: (Ord k, Binary k) => Index -> k -> Tag -> Maybe Lazy.ByteString -> IO [(Tag,Maybe Strict.ByteString)]
insertBS idx key tag mbVal
    = insertWithBS idx key tag (\_ -> return mbVal)

{- SPECIALISE insertWithBS :: Index -> Strict.ByteString -> Tag -> (Ptr DataCursor -> IO (Maybe Lazy.ByteString)) -> IO [(Tag,Maybe Strict.ByteString)] -}
insertWithBS :: (Ord k, Binary k) => Index -> k -> Tag -> (Ptr DataCursor -> IO (Maybe Lazy.ByteString)) -> IO [(Tag,Maybe Strict.ByteString)]
insertWithBS (Index orig buffer) key tag genVal
    = do keyCursor <- insertKey (Index orig buffer) key
         oldData   <- peekKeyCursorData keyCursor            -- Get the old data item
         dataPtr <- newDataCursor buffer tag =<< genVal oldData
         pushNewDataCursor keyCursor dataPtr
         if oldData == nullPtr
            then return []
            else fetchAllElts oldData

{-# INLINE insertKey #-}
insertKey :: (Ord k, Binary k) => Index -> k -> IO (Ptr KeyCursor)
insertKey (Index orig buffer) key
    = insertPrim (lookupNearest orig key) orig buffer (newBinaryKeyCursor buffer key)

{-# INLINE insertLargestKey #-}
insertLargestKey :: (Binary k) => Index -> k -> IO (Ptr KeyCursor)
insertLargestKey (Index orig buffer) key
    = insertPrim (lookupLargest orig) orig buffer (newBinaryKeyCursor buffer key)

insertLargestKeyCursor :: Index -> Ptr KeyCursor -> IO ()
insertLargestKeyCursor (Index orig buffer) keyCursor
    = do insertPrim (lookupLargest orig) orig buffer (return keyCursor)
         return ()

lookupKey :: (Ord k, Binary k) => Index -> k -> IO (Maybe (Ptr KeyCursor))
lookupKey (Index orig buffer) key
    = do (dir,pos) <- lookupNearest orig key
         case dir of
           Stop -> fmap Just $ extractElemIdx pos
           _    -> return Nothing

lookupList :: (Ord k, Binary k) => Index -> k -> IO [(Tag,Maybe Strict.ByteString)]
lookupList idx key
    = do mbKey <- lookupKey idx key
         case mbKey of
           Nothing  -> return []
           Just key -> fetchAllElts =<< peekKeyCursorData key

fetchAllElts :: Ptr DataCursor -> IO [(Tag,Maybe Strict.ByteString)]
fetchAllElts ptr | ptr == nullPtr = return []
fetchAllElts ptr
    = unsafeInterleaveIO $
      do next <- peekDataCursorNext ptr
         tag  <- peekDataCursorTag ptr
         mbData <- peekDataCursorData ptr
         liftM ((tag,mbData):) (fetchAllElts next)

indexItemSize :: Int
indexItemSize = sizeOf (undefined :: IndexItem)

{-
  Insert a key in the map. Return pointer to the old key if it exists.
-}
{- SPECIALISE insertPrim :: IO (Direction,Ptr IndexItem) -> Ptr IndexItem -> Buffer -> IO (Ptr KeyCursor) -> IO (Ptr KeyCursor) -}
{-# INLINE insertPrim #-}
insertPrim :: (IO (Direction,Ptr IndexItem)) -> Ptr IndexItem -> Buffer -> IO (Ptr KeyCursor) -> IO (Ptr KeyCursor)
insertPrim getPos !orig !buffer genIdx
    = do size <- getSize orig
         if size==0 -- We need a special case for size=0 /-:
            then do eIdx <- genIdx
                    poke orig (IndexItem nullPtr (intToPtr 1) eIdx nullPtr nullPtr)
                    return eIdx
            else do (dir,pos) <- getPos
                    case dir of
                      Right -> withBytes buffer indexItemSize $ \ptr ->
                               do eIdx <- genIdx
                                  poke ptr (IndexItem pos (intToPtr 1) eIdx nullPtr nullPtr)
                                  putRight pos ptr
                                  balanceTree pos
                                  return eIdx
                      Left -> withBytes buffer indexItemSize $ \ptr ->
                              do eIdx <- genIdx
                                 poke ptr (IndexItem pos (intToPtr 1) eIdx nullPtr nullPtr)
                                 putLeft pos ptr
                                 balanceTree pos
                                 return eIdx
                      Stop -> extractElemIdx pos



listKeyPointers :: Index -> IO (UArray Int (Ptr KeyCursor))
listKeyPointers (Index orig buffer)
    = do size <- getSize orig
         a <- newArray_ (0,size-1) :: IO (IOUArray Int (Ptr KeyCursor))
         let loop n ptr | ptr == nullPtr = return ()
             loop n ptr = do left <- extractLeft ptr
                             right <- extractRight ptr
                             leftSize <- getSize left
                             key  <- extractElemIdx ptr
                             writeArray a (leftSize+n) key
                             loop (n) left
                             loop (leftSize+1+n) right
         unless (size==0) $ loop (0::Int) orig
         unsafeFreeze a

getKeyFromPointer :: Ptr KeyCursor -> IO Strict.ByteString
getKeyFromPointer ptr
    = peekKeyCursorKey ptr

getDataFromPointer :: Ptr KeyCursor -> IO [(Tag, Maybe Strict.ByteString)]
getDataFromPointer ptr
    = do dataPtr <- peekKeyCursorData ptr
         fetchAllElts dataPtr


{-
getAllElements fn (Index buffer)
    = do elems <- readBufferPos buffer
         indices <- if elems == 0 then return id else sumIndex 0 buffer
         vals <- forM (indices []) $ \idx -> sumIndex idx buffer
         return $ concatMap ($ []) vals
    where sumIndex = foldIndex sumNode sumLeaf
          sumNode _ _ idx left right = return $ fn idx left right
          sumLeaf = return id

getSortedElements = getAllElements (\idx left right -> left . (idx:) . right)
getReverseElements = getAllElements (\idx left right -> right . (idx:) . left)
-}

newIndex = do buffer <- newBuffer 512
              withBytes buffer indexItemSize $ \ptr -> ptr `seq`
                do poke ptr (IndexItem nullPtr (intToPtr 0) nullPtr nullPtr nullPtr)
                   return $ Index ptr buffer

touchIndex (Index _ buffer) = touchBuffer buffer
{-


foldIndex node leaf start buffer
    = do ptr <- bufferPtr buffer
         let loop (-1) = leaf
             loop n = do IndexItem size idx left right <- peekElemOff ptr n
                         restLeft <- loop left
                         restRight <- loop right
                         node n size idx restLeft restRight
         loop start

showPrimIndex set
    = do let Index buffer = (fromJust (tIndex set))
         let leaf = return []
             node n _size _idx left right
                 = return $ n:left++right
         values <- foldIndex node leaf 0 buffer
         free <- readBufferPos buffer
         printf " \tSize\tIndex\tLeft\tRight\n"
         ptr <- bufferPtr buffer
         forM_ [0..free-1] $ \n ->
             do IndexItem size idx left right <- peekElemOff ptr n
                let isntValue = n `notElem` values
                printf "%s%d\t%d\t%d\t%d\t%d\n" (if isntValue then "*" else " ") n size idx left right

showIndex set
    = do let Index buffer = (fromJust (tIndex set))
         elems <- readBufferPos buffer
         ptr <- bufferPtr buffer
         let leaf = return $ [Node "Leaf" []]
             node _ size idx restLeft restRight
                 = do eIdx <- return idx -- getElement idx -- set =<< extractElemIdx ptr idx
                      positions <- foldIndex (\_ _ pos left right -> return [Node pos (left++right)]) (return []) idx buffer
                      return $ [Node (show eIdx ++ ": " ++ show positions) (restLeft++restRight)]
         unless (elems==0)
                    $ do tree <- foldIndex node leaf 0 buffer
                         putStrLn (drawForest tree)

{- SPECIALIZE isValid :: DiskSet RawString -> IO () -}
-- isValid :: IO ()
isValid set
    = do let Index buffer = (fromJust (tIndex set))
         elems <- readBufferPos buffer
         ptr <- bufferPtr buffer
         let check True _ = return ()
             check False msg = putStrLn msg >> exitWith (ExitFailure 1)
             leaf = return (Nothing, 0)
             node n size idx (mbLeft, leftN) (mbRight, rightN)
                 = do eIdx <- return idx -- getElement idx -- set =<< extractElemIdx ptr idx
                      check (leftN+rightN+1 == size) $ "Size check failed at " ++ show (n,size,leftN,rightN)
                      flip (maybe (return ())) mbLeft $ \el -> check (el < eIdx) $ "LT check failed at: " ++ show (idx,el,eIdx)
                      flip (maybe (return ())) mbRight $ \el -> check (el > eIdx) $ "GT check failed at: " ++ show (idx,el,eIdx)
                      return (Just eIdx, size)
         unless (elems==0) $ foldIndex node leaf 0 buffer >> return ()
         return ()
-}
{-
testSet :: [String]
testSet = [("Hello")
          ,("World")
          ,("This")
          ,("Is")
          ,("A Test")
          ,("Yay")
          ,("I think it works")]

test :: IO ()
test = do idx <- newIndex
          forM_ (IntMap.keys testSet) $ \key -> do insert testSet idx key
                                                   balanceIndex idx
          showIndex testSet idx
          isValid testSet idx
          putStrLn "Index is valid"
-}
{-
verify prev !pos | pos == nullPtr = return ()
verify prev !pos
    = do !top <- extractTop pos
         !left <- extractLeft pos
         !right <- extractRight pos
         !sizeL <- getSize left
         !sizeR <- getSize right
         size <- getSize pos
         unless (size==sizeL+sizeR+1) $ putStrLn $ "Size fail: " ++ show (size,sizeL,sizeR)
         unless (top==prev) $ putStrLn "Top fail"
         verify pos left
         verify pos right
-}
balanceTree !pos | pos==nullPtr = return ()
balanceTree !pos
    = do balance pos
         !top <- extractTop pos
         balanceTree top

getSize pos | pos == nullPtr = return $! 0
getSize pos
    = extractSize pos

balance !pos
    = do --keyCursor <- extractElemIdx pos
         --bs <- peekKeyCursorKey keyCursor
         --putStrLn $ "Balancing: " ++ show (decode (Lazy.fromChunks [bs]) :: Integer)
         !left <- extractLeft pos
         !right <- extractRight pos
         !sizeL <- getSize left
         !sizeR <- getSize right
         putSize pos (sizeL+sizeR+1)
         case () of
           () | sizeL + sizeR <= 1   -> return ()
              | sizeR >= delta*sizeL -> rotateL pos left right
              | sizeL >= delta*sizeR -> rotateR pos left right
              | otherwise            -> return ()

rotateL pos left right
    = do !sizeLY <- getSize =<< extractLeft right
         !sizeRY <- getSize =<< extractRight right
         if sizeLY < ratio * sizeRY then singleL pos
                                    else doubleL pos

rotateR pos left right
    = do !sizeLY <- getSize =<< extractLeft left
         !sizeRY <- getSize =<< extractRight left
         if sizeRY < ratio * sizeLY then singleR pos
                                    else doubleR pos
singleL pos
    = do IndexItem kTop kSize kElemIdx p1 k2 <- peek pos
         IndexItem k2Top k2Size k2ElemIdx p2 p3 <- peek k2
         --unless (k2Top == pos) $ putStrLn "Assertion failure"
         !p2Size <- getSize p2
         let p1Size = ptrToInt kSize-ptrToInt k2Size-1
         poke pos (IndexItem kTop kSize k2ElemIdx k2 p3) -- kSize hasn't changed
         poke k2 (IndexItem k2Top (intToPtr $ p2Size+p1Size+1) kElemIdx p1 p2)
         putTop p3 pos
         putTop p1 k2
--         recalcSize ptr k2 p1 p2

singleR pos
    = do IndexItem kTop kSize kElemIdx k2 p3 <- peek pos
         IndexItem k2Top k2Size k2ElemIdx p1 p2 <- peek k2
         --unless (k2Top == pos) $ putStrLn "Assertion failure"
         !p2Size <- getSize p2
         let p3Size = ptrToInt kSize-ptrToInt k2Size-1
         poke pos (IndexItem kTop kSize k2ElemIdx p1 k2) -- kSize hasn't changed
         poke k2 (IndexItem k2Top (intToPtr $ p2Size+p3Size+1) kElemIdx p2 p3)
         putTop p1 pos
         putTop p3 k2
--         recalcSize ptr k2 p2 p3

doubleL pos
    = do IndexItem kTop kSize kElemIdx p1 k2 <- peek pos
         IndexItem k2Top k2Size k2ElemIdx k3 p4 <- peek k2
         IndexItem k3Top k3Size k3ElemIdx p2 p3 <- peek k3
--         putStrLn "doubleL"
         !p2Size <- getSize p2
         !p3Size <- getSize p3
         let p1Size = ptrToInt kSize - ptrToInt k2Size - 1
             p4Size = ptrToInt k2Size - ptrToInt k3Size - 1
         poke pos (IndexItem kTop kSize k3ElemIdx k3 k2) -- kSize hasn't changed
         poke k2 (IndexItem k2Top (intToPtr $ p3Size+p4Size+1) k2ElemIdx p3 p4) -- k2ElemIdx and p4 hasn't changed
         poke k3 (IndexItem k2Top (intToPtr $ p1Size+p2Size+1) kElemIdx p1 p2)
         putTop p1 k3
         putTop k3 pos
         putTop p3 k2

doubleR pos
    = do IndexItem kTop kSize kElemIdx k2 p4 <- peek pos
         IndexItem k2Top k2Size k2ElemIdx p1 k3 <- peek k2
         IndexItem k3Top k3Size k3ElemIdx p2 p3 <- peek k3
--         putStrLn "doubleR"
         !p2Size <- getSize p2
         !p3Size <- getSize p3
         let p1Size = ptrToInt k2Size - ptrToInt k3Size - 1
             p4Size = ptrToInt kSize - ptrToInt k2Size - 1
         poke pos (IndexItem kTop kSize k3ElemIdx k2 k3) -- kSize hasn't changed
         poke k2 (IndexItem pos (intToPtr $ p1Size+p2Size+1) k2ElemIdx p1 p2) -- k2ElemIdx and p1 hasn't changed.
         poke k3 (IndexItem pos (intToPtr $ p3Size+p4Size+1) kElemIdx p3 p4)
         putTop k3 pos
         putTop p2 k2
         putTop p4 k3

{-
recalcSize ptr !pos !left !right
    = do !sizeL <- getSize ptr left
         !sizeR <- getSize ptr right
         putSize ptr pos (sizeL+sizeR+1)
-}

delta,ratio :: Int
delta = 5
ratio = 2

{-
balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sizeL + sizeR <= 1    = Bin sizeX k x l r
  | sizeR >= delta*sizeL  = rotateL k x l r
  | sizeL >= delta*sizeR  = rotateR k x l r
  | otherwise             = Bin sizeX k x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1

-- rotate
rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < ratio*size ry = singleL k x l r
  | otherwise               = doubleL k x l r

rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < ratio*size ly = singleR k x l r
  | otherwise               = doubleR k x l r

-- basic rotations
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)

doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)

-}

