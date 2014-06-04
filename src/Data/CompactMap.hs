{-# LANGUAGE NoBangPatterns, CPP, DeriveDataTypeable, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CompactMap
-- Copyright   :  (c) David Himmelstrup 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- An efficient implementation of maps from keys to values (dictionaries).
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.CompactMap (Map)
-- >  import qualified Data.CompactMap as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--      Journal of Functional Programming 3(4):553-562, October 1993,
--      <http://www.swiss.ai.mit.edu/~adams/BB/>.
--
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-----------------------------------------------------------------------------
module Data.CompactMap
    ( -- * Map type
      Map          -- instance Eq,Show,Read
      
      -- * Operators
    , (!) --, (\\)
     
     
      -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
      
      -- * Construction
    , empty
    , singleton
      
      -- ** Insertion
    , insert
    , insertWith, insertWithKey, insertLookupWithKey
      
      -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter

      -- * Combine
    , union         
    , unionWith
    , unionWithKey
    , unions
    , unionsWith{-
      
      -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey
      
      -- ** Intersection
    , intersection           
    , intersectionWith
    , intersectionWithKey-}
      
      -- * Traversal
      -- ** Map
    , map
    , mapWithKey{-
    , mapAccum
    , mapAccumWithKey-}
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

      -- ** Fold
    , fold
    , foldWithKey
      
      -- * Conversion
    , elems
    , keys
    , keysSet
    , assocs

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

      -- ** Ordered lists
    , toAscList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList

      -- * Filter 
    , filter
    , filterWithKey
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey{-
      
    , split         
    , splitLookup   
      
      -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy
      
      -- * Indexed 
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt-}
      -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
--    , deleteMax
    , deleteFindMin
--    , deleteFindMax
{-    , updateMin
    , updateMax
    , updateMinWithKey
    , updateMaxWithKey
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey
      
      -- * Debugging
    , showTree
    , showTreeWith
    , valid-}
    ) where

      
import Data.Monoid (Monoid(..))
import Control.Concurrent
import Data.IORef
import Data.Binary
import Data.Typeable
import Data.List (foldl')
import System.IO.Unsafe

import qualified Data.Maybe as Maybe
import Data.Maybe (isJust)
import Foreign (nullPtr)
import Text.Read hiding (get)
import Control.Monad
import qualified Data.CompactMap.Index as Index
import Data.CompactMap.Types as Types
import qualified Data.Array.IArray as IArray
import qualified Data.Set as Set

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

import Prelude hiding (null,lookup,map,filter)
import qualified Prelude

import System.Mem.Weak

data Range = Range Int Int

-- | A Map from keys @k@ to values @a@.
data Map k a = Empty
             | Existing 
               { index   :: !(MVar Index)
               , uniq    :: {-# UNPACK #-} !(IORef Int)
               , range   :: ![Range]
               , mapSize :: {-# UNPACK #-} !Int
               }
#if !defined(HPC)
                deriving (Typeable)
#endif

{--------------------------------------------------------------------
  Instances
--------------------------------------------------------------------}

instance (Eq k, Eq a, Binary k, Binary a) => Eq (Map k a) where
    m1 == m2 = toList m1 == toList m2

instance (Ord k, Ord a, Binary k, Binary a) => Ord (Map k a) where
    m1 `compare` m2 = toList m1 `compare` toList m2

instance (Binary k, Binary a, Show k, Show a) => Show (Map k a) where
        showsPrec d m  = showParen (d > 10) $
                         showString "fromList " . shows (toList m)
        
instance (Ord k, Binary k, Binary a, Read k, Read a) => Read (Map k a) where
#ifdef __GLASGOW_HASKELL__
     readPrec = parens $ prec 10 $ do
                  Ident "fromList" <- lexP
                  xs <- readPrec
                  return (fromList xs)
                
     readListPrec = readListPrecDefault
#else
     readsPrec p = readParen (p > 10) $ \ r -> do
                      ("fromList",s) <- lex r
                      (xs,t) <- reads s
                      return (fromList xs,t)
#endif

instance Binary (Map k a) where
    put Empty = put (0::Int)
    put Existing{index=index,range=range,mapSize=mapSize} =
         let a = unsafePerformIO $ withMVar index $ Index.listKeyPointers
         in do put mapSize
               forM_ (IArray.elems a) $ \ptr ->
                 do let ls = unsafePerformIO $ Index.getDataFromPointer ptr
                    case findValue range ls of
                      Nothing  -> return ()
                      Just val -> do let key = unsafePerformIO $ Index.getKeyFromPointer ptr
                                     put (key,val)
               unsafePerformIO $
                 do withMVar index Index.touchIndex
                    return $ return ()
    get = do n <- get
             ls <- replicateM n get
             unsafePerformIO $
               do idx <- Index.newIndex
                  forM_ ls $ \(k,v) -> do keyCursor <- Index.newKeyCursor (indexBuffer idx) (Lazy.fromChunks [k])
                                          Index.insertLargestKeyCursor idx keyCursor
                                          dataCursor <- Index.newDataCursor (indexBuffer idx) 0 (Just (Lazy.fromChunks [v]))
                                          Index.pushNewDataCursor keyCursor dataCursor
                                          --Index.insertBS idx (decodeStrict k :: k) 0 (Just (Lazy.fromChunks [v]))
                  uniq <- newIORef 1
                  index <- newMVar idx
                  return $ return $ Existing{index=index,uniq=uniq,range=addToRange 0 [],mapSize=n}

instance (Ord k, Binary k, Binary a) => Monoid (Map k a) where
  mempty  = empty
  mappend = union
  mconcat = unions
        

{--------------------------------------------------------------------
  Methods
--------------------------------------------------------------------}

infixl 9 ! -- ,\\

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: (Ord k, Binary k, Binary a) => Map k a -> k -> a
m ! k = case lookup k m of
          Nothing -> error "element not in the map"
          Just x  -> x


-- | /O(1)/. Is the map empty?
--
-- > Data.Map.null (empty)           == True
-- > Data.Map.null (singleton 1 'a') == False
null :: Map k a -> Bool
null m = size m == 0

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: Map k a -> Int
size Empty = 0
size Existing{mapSize=size} = size

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: (Ord k, Binary k) => k -> Map k a -> Bool
member k Empty = False
member k Existing{index=index,range=range}
    = unsafePerformIO $ withMVar index $ \idx ->
      do ls <- Index.lookupList idx k
         return $ isJust $ findValue range ls

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
notMember :: (Ord k, Binary k) => k -> Map k a -> Bool
notMember k m = not (member k m)

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.CompactMap
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup :: (Ord k, Binary k, Binary a) => k -> Map k a -> Maybe a
lookup k Empty = Nothing
lookup k Existing{index=index,range=range}
    = unsafePerformIO $ withMVar index $ \idx ->
      do ls <- Index.lookupList idx k
         case findValue range ls of
           Nothing -> return Nothing
           Just bs -> do mkWeak bs index Nothing
                         return $ Just (decodeStrict bs)

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: (Ord k, Binary k, Binary a) => a -> k -> Map k a -> a
findWithDefault def k m = case lookup k m of
                            Nothing -> def
                            Just x  -> x

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: Map k a
empty = Empty

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: (Ord k, Binary k, Binary a) => k -> a -> Map k a
singleton k a = insert k a empty

-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'
insert :: (Ord k, Binary k, Binary a) => k -> a -> Map k a -> Map k a
insert k a m
    = unsafePerformIO $
      withExisting m $ \Existing{index=index,uniq=uniq,range=range,mapSize=mapSize} ->
      withMVar index $ \idx ->
      do u <- readIORef uniq
         modifyIORef uniq succ
         ls <- Index.insert idx k u (Just a)
         let newSize | haveOldValue range ls = mapSize
                     | otherwise             = mapSize+1
         return Existing{index=index,uniq=uniq,range=addToRange u range,mapSize=newSize}

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: (Ord k, Binary k, Binary a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k x m
    = insertWithKey (\_ x' y' -> f x' y') k x m

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
insertWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f kx x Empty = singleton kx x
insertWithKey f kx x Existing{index=index,uniq=uniq,range=range,mapSize=mapSize}
    = unsafePerformIO $ withMVar index $ \idx ->
      do u <- readIORef uniq
         modifyIORef uniq succ
         keyCursor <- Index.insertKey idx kx
         ls <- Index.getDataFromPointer keyCursor
         let oldVal = findValue range ls
             newVal = case oldVal of
                        Nothing  -> x
                        Just old -> f kx x (decodeStrict old)
             newSize = if isJust oldVal then mapSize else mapSize + 1
         dataCursor <- Index.newDataCursor (indexBuffer idx) u (Just $ encode newVal)
         Index.pushNewDataCursor keyCursor dataCursor
         return $ Existing{index=index,uniq=uniq,range=addToRange u range,mapSize=newSize}

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])
insertLookupWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a,Map k a)
insertLookupWithKey f k a Empty = (Nothing, singleton k a)
insertLookupWithKey f k a Existing{index=index,uniq=uniq,range=range,mapSize=mapSize}
    = unsafePerformIO $ withMVar index $ \idx ->
      do u <- readIORef uniq
         modifyIORef uniq succ
         keyCursor <- Index.insertKey idx k
         ls <- Index.getDataFromPointer keyCursor
         let oldValBS = findValue range ls
             oldVal = fmap decodeStrict oldValBS
             newVal = case oldVal of
                        Nothing  -> a
                        Just old -> f k a old
             newSize = if isJust oldVal then mapSize else mapSize + 1
         case oldValBS of Just val -> mkWeak val index Nothing>>return(); Nothing -> return ()
         dataCursor <- Index.newDataCursor (indexBuffer idx) u (Just $ encode newVal)
         Index.pushNewDataCursor keyCursor dataCursor
         return $ (oldVal, Existing{index=index,uniq=uniq,range=addToRange u range,mapSize=newSize})
         


-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty
delete :: (Ord k, Binary k) => k -> Map k a -> Map k a
delete k Empty = Empty
delete k Existing{index=index,uniq=uniq,range=range,mapSize=mapSize}
    = unsafePerformIO $ withMVar index $ \idx ->
      do u <- readIORef uniq
         modifyIORef uniq succ
         ls <- Index.insert idx k u (Nothing :: Maybe ())
         let newSize | haveOldValue range ls = mapSize-1
                     | otherwise             = mapSize
         return Existing{index=index,uniq=uniq,range=addToRange u range,mapSize=newSize}

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty
adjust :: (Ord k, Binary k, Binary a) => (a -> a) -> k -> Map k a -> Map k a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty
adjustWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k m
  = updateWithKey (\k' x' -> Just (f k' x')) k m

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
update :: (Ord k, Binary k, Binary a) => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k m
  = updateWithKey (\_ x -> f x) k m

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
updateWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k m = snd (updateLookupWithKey f k m)

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted. 
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
updateLookupWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a,Map k a)
updateLookupWithKey f k Empty = (Nothing,Empty)
updateLookupWithKey f k m@Existing{index=index,uniq=uniq,range=range,mapSize=mapSize}
    = unsafePerformIO $ withMVar index $ \idx ->
      do ls <- Index.lookupList idx k
         case findValue range ls of
           Nothing  -> return (Nothing, m)
           Just valBS -> do let val = decodeStrict valBS
                                newVal = f k val
                            mkWeak valBS index Nothing
                            u <- readIORef uniq
                            modifyIORef uniq succ
                            Index.insert idx k u newVal
                            let newSize = case isJust newVal of
                                  False -> mapSize-1
                                  True  -> mapSize
                            return (newVal `mplus` Just val, Existing{index=index,uniq=uniq,range=addToRange u range,mapSize=newSize})


-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]
alter :: (Ord k, Binary k, Binary a) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k m
    = case f (lookup k m) of
        Nothing -> delete k m
        Just val -> insert k val m

-- | /O(log n*m)/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@. 
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
union :: (Ord k, Binary k, Binary a) => Map k a -> Map k a -> Map k a
union = unionWith const

-- | /O(log n*m)/. Union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
unionWith :: (Ord k, Binary k, Binary a) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f m1 m2
    = unionWithKey (\_ x y -> f x y) m1 m2
        

-- | /O(log n*m)/.
-- Union with a combining function.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
unionWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f t1 t2 = foldl' (\m (k,v) -> insertWithKey f k v m) t2 (toList t1)

-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]
unions :: (Ord k, Binary k, Binary a) => [Map k a] -> Map k a
unions ts
  = foldl' union empty ts

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
unionsWith :: (Ord k, Binary k, Binary a) => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f ts
  = foldl' (unionWith f) empty ts

-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: (Ord k, Binary k, Binary a, Binary b) => (a -> b) -> Map k a -> Map k b
map f m
    = mapWithKey (\_ x -> f x) m

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: (Ord k, Binary k, Binary a, Binary b) => (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = fromDistinctAscList [ (k, f k x) | (k,x) <- toList m ]

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the smallest of
-- these keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"
mapKeys :: (Ord k2,Binary k1,Binary k2,Binary a) => (k1->k2) -> Map k1 a -> Map k2 a
mapKeys = mapKeysWith (\x _ -> x)

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"
mapKeysWith :: (Ord k2, Binary k1, Binary k2, Binary a) => (a -> a -> a) -> (k1->k2) -> Map k1 a -> Map k2 a
mapKeysWith c f m = fromListWith c [ (f x,y) | (x,y) <- toList m ]

-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls] 
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False
mapKeysMonotonic :: (Binary k1, Binary k2, Binary a) => (k1->k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic f m = fromDistinctAscList [ (f x, y) | (x,y) <- toList m ]

-- | /O(n)/. Fold the values in the map, such that
-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
-- For example,
--
-- > elems map = fold (:) [] map
--
-- > let f a len = len + (length a)
-- > fold f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
fold :: (Binary k, Binary a) => (a -> b -> b) -> b -> Map k a -> b
fold f z m
  = foldWithKey (\_ x' z' -> f x' z') z m

-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldWithKey :: (Binary k, Binary a) => (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey f z = Prelude.foldr (uncurry f) z . toList


-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []
elems :: (Binary k, Binary a) => Map k a -> [a]
elems = Prelude.map snd . toList

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []
keys  :: (Binary k, Binary a) => Map k a -> [k]
keys = Prelude.map fst . toList

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty
keysSet :: (Ord k, Binary k, Binary a) => Map k a -> Set.Set k
keysSet m = Set.fromDistinctAscList (keys m)

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []
assocs :: (Binary k, Binary a) => Map k a -> [(k,a)]
assocs m
  = toList m



{-# SPECIALISE fromList :: (Binary a) => [(Strict.ByteString,a)] -> Map Strict.ByteString a #-}
{-# SPECIALISE fromList :: (Binary a) => [(Int,a)] -> Map Int a #-}
-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
fromList :: (Ord k, Binary k, Binary a) => [(k,a)] -> Map k a
fromList [] = Empty
fromList ls
    = unsafePerformIO $
      do idx <- Index.newIndex
         let loop n _ | n `seq` False = undefined
             loop n [] = return n
             loop n ((k,v):rs) | k `seq` v `seq` True
               = do keyCursor <- Index.insertKey idx k
                    oldData   <- Index.peekKeyCursorData keyCursor
                    newData   <- Index.newDataCursor (indexBuffer idx) 0 (Just (encode v))
                    Index.pushNewDataCursor keyCursor newData
                    loop (if oldData==nullPtr then n+1 else n) rs
         size <- loop 0 ls
         uniq <- newIORef 1
         index <- newMVar idx
         return $ Existing{index=index,uniq=uniq,range=addToRange 0 [],mapSize=size}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty
fromListWith :: (Ord k, Binary k, Binary a) => (a -> a -> a) -> [(k,a)] -> Map k a 
fromListWith f xs
    = fromListWithKey (\_ x y -> f x y) xs

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- > fromListWithKey f [] == empty
fromListWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromListWithKey f [] = empty
fromListWithKey f ls
    = unsafePerformIO $
      do idx <- Index.newIndex
         let loop n _ | n `seq` False = undefined
             loop n [] = return n
             loop n ((k,v):rs)
               = do keyCursor <- Index.insertKey idx k
                    oldData   <- Index.getDataFromPointer keyCursor
                    let newVal = case oldData of
                                   ((_,Just old):_) -> f k v (decodeStrict old)
                                   _  -> v
                    newData   <- Index.newDataCursor (indexBuffer idx) 0 (Just (encode newVal))
                    Index.pushNewDataCursor keyCursor newData
                    loop (if Prelude.null oldData then n+1 else n) rs
         size <- loop 0 ls
         uniq <- newIORef 1
         index <- newMVar idx
         return $ Existing{index=index,uniq=uniq,range=addToRange 0 [],mapSize=size}


-- | /O(n)/. Convert to a list of key\/value pairs.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []
toList :: (Binary k, Binary a) => Map k a -> [(k,a)]
toList Empty = []
toList Existing{index=index,range=range}
    = unsafePerformIO $
      do keys <- withMVar index $ Index.listKeyPointers
         let loop [] = return [] -- withMVar index Index.touchIndex >> return []
             loop (keyCursor:xs)
                     = unsafeInterleaveIO $
                       do mkWeak keyCursor index Nothing
                          ls <- Index.getDataFromPointer keyCursor
                          case findValue range ls of
                            Nothing -> loop xs
                            Just bs -> do key <- Index.getKeyFromPointer keyCursor
                                          mkWeak bs index Nothing
                                          mkWeak key index Nothing
                                          let pair = (decodeStrict key, decodeStrict bs)
                                          liftM (pair:) (loop xs)
         loop (IArray.elems keys)


-- | /O(n)/. Convert to an ascending list.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
toAscList :: (Binary k, Binary a) =>Map k a -> [(k,a)]
toAscList = toList

-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False
fromAscList :: (Eq k, Binary k, Binary a) => [(k,a)] -> Map k a 
fromAscList xs
    = fromAscListWithKey (\_ x _ -> x) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False
fromAscListWith :: (Eq k, Binary k, Binary a) => (a -> a -> a) -> [(k,a)] -> Map k a 
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False
fromAscListWithKey :: (Eq k, Binary k, Binary a) => (k -> a -> a -> a) -> [(k,a)] -> Map k a 
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,zz) (x@(kx,xx):xs')
    | kx==kz    = let yy = f kx xx zz in combineEq' (kx,yy) xs'
    | otherwise = z:combineEq' x xs'


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False
fromDistinctAscList :: (Binary k, Binary a) => [(k,a)] -> Map k a
fromDistinctAscList [] = Empty
fromDistinctAscList ls
    = unsafePerformIO $
      do idx <- Index.newIndex
         n <- foldM (\s (k,v) -> do keyCursor <- Index.insertLargestKey idx k
                                    dataCursor <- Index.newDataCursor (indexBuffer idx) 0 (Just $ encode v)
                                    Index.pushNewDataCursor keyCursor dataCursor
                                    return $! s+1) 0 ls
         index <- newMVar idx
         uniq <- newIORef 1
         return Existing{index=index,uniq=uniq,range=addToRange 0 [],mapSize=n}


-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty
filter :: (Ord k, Binary k, Binary a) => (a -> Bool) -> Map k a -> Map k a
filter p m
    = filterWithKey (\_ x -> p x) m

-- FIXME: optimize this.
-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
filterWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey p m = fromDistinctAscList [ (k, v) | (k,v) <- toList m, p k v ]

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partition :: (Ord k, Binary k, Binary a) => (a -> Bool) -> Map k a -> (Map k a,Map k a)
partition p = partitionWithKey (\_ -> p)

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionWithKey :: (Ord k, Binary k, Binary a) => (k -> a -> Bool) -> Map k a -> (Map k a,Map k a)
partitionWithKey p m = mapEitherWithKey (\k x -> if p k x then Left x else Right x) m

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
mapMaybe :: (Ord k, Binary k, Binary a, Binary b) => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f m
    = mapMaybeWithKey (\_ x -> f x) m

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
mapMaybeWithKey :: (Ord k, Binary k, Binary a, Binary b) => (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f m = fromDistinctAscList [ (k, v) | (k,x) <- toList m, Just v <- [f k x] ]


-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (Ord k, Binary k, Binary a, Binary b, Binary c) => (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- The key doesn't change. Don't re-encode it. Copy bytestring instead.
-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
mapEitherWithKey :: (Ord k, Binary k, Binary a, Binary c, Binary b) =>
  (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f m
    = unsafePerformIO $
      do idxL <- Index.newIndex
         idxR <- Index.newIndex
         (s1,s2) <- foldM (\(s1,s2) (k,v) -> s1 `seq` s2 `seq`
                                             do let cond = f k v
                                                    (idx,v',s1',s2') = case cond of
                                                                         Left v'  -> (idxL,encode v',s1+1,s2)
                                                                         Right v' -> (idxR,encode v',s1,s2+1)
                                                keyCursor <- Index.insertLargestKey idx k
                                                dataCursor <- Index.newDataCursor (indexBuffer idx) 0 (Just v')
                                                Index.pushNewDataCursor keyCursor dataCursor
                                                return $! (s1',s2')) (0,0) (toList m)
         indexL <- newMVar idxL
         indexR <- newMVar idxR
         uniqL  <- newIORef 1
         uniqR  <- newIORef 1
         return $ (Existing{index=indexL,uniq=uniqR,range=addToRange 0 [],mapSize=s1}
                  ,Existing{index=indexR,uniq=uniqL,range=addToRange 0 [],mapSize=s2})


-- | /O(log n)/. The minimal key of the map. Calls 'error' is the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element
findMin :: (Binary k, Binary a) => Map k a -> (k,a)
findMin m = case m of
             Empty -> err
             Existing{index=index,range=range} ->
               unsafePerformIO $ withMVar index $ \idx ->
               do mbMin <- findMinKey idx range
                  case mbMin of
                    Nothing -> err
                    Just (keyCursor,val)
                      -> do key <- Index.getKeyFromPointer keyCursor
                            mkWeak key index Nothing
                            mkWeak val index Nothing
                            return (decodeStrict key, decodeStrict val)
    where err = error "Map.findMin: empty map has no minimal element"

-- | /O(log n)/. The maximal key of the map. Calls 'error' is the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element
findMax :: (Binary k, Binary a) => Map k a -> (k,a)
findMax m = case m of
             Empty -> err
             Existing{index=index,range=range} ->
               unsafePerformIO $ withMVar index $ \idx ->
               do mbMin <- findMaxKey idx range
                  case mbMin of
                    Nothing -> err
                    Just (keyCursor,val)
                      -> do key <- Index.getKeyFromPointer keyCursor
                            mkWeak key index Nothing
                            mkWeak val index Nothing
                            return (decodeStrict key, decodeStrict val)
    where err = error "Map.findMax: empty map has no maximal element"

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty
deleteMin :: (Binary k, Binary a) => Map k a -> Map k a
deleteMin = snd . deleteFindMin

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")]) 
-- > deleteFindMin empty                                   == (Error: can not return the minimal element of an empty map,empty)
deleteFindMin :: (Binary k, Binary a) => Map k a -> ((k,a), Map k a)
deleteFindMin m
    = case m of
        Empty -> (deleteFindMinErr, Empty)
        Existing{index=index,uniq=uniq,range=range,mapSize=mapSize}
          -> unsafePerformIO $ withMVar index $ \idx ->
             do mbMin <- findMinKey idx range
                case mbMin of
                  Nothing -> return (deleteFindMinErr, Empty)
                  Just (keyCursor,val)
                    -> do u <- readIORef uniq
                          modifyIORef uniq succ
                          dataCursor <- Index.newDataCursor (indexBuffer idx) u Nothing
                          Index.pushNewDataCursor keyCursor dataCursor
                          key <- Index.getKeyFromPointer keyCursor
                          mkWeak key index Nothing
                          mkWeak val index Nothing
                          return $ ((decodeStrict key,decodeStrict val),Existing{index=index,uniq=uniq,range=addToRange u range,mapSize=mapSize-1})
    where deleteFindMinErr = error "Data.CompactMap.deleteFindMin: can not return the minimal element of an empty map"

findMinKey = findCornerKey Index.extractLeft Index.extractRight
findMaxKey = findCornerKey Index.extractRight Index.extractLeft
findCornerKey left right (Index orig buffer) range
    = do s <- Index.getSize orig
         if s == 0
            then return Nothing
            else do let loop ptr | ptr == nullPtr = return Nothing
                        loop ptr = do res <- loop =<< left ptr
                                      case res of
                                        Just val -> return $ Just val
                                        Nothing -> do keyCursor <- Index.extractElemIdx ptr
                                                      ls <- Index.getDataFromPointer keyCursor
                                                      case findValue range ls of
                                                        Just val -> return $ Just (keyCursor, val)
                                                        _ -> loop =<< right ptr
                    loop orig


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

decodeStrict bs = decode (Lazy.fromChunks [bs])

haveOldValue range ls
    = isJust (findValue range ls)

withExisting Empty fn
    = do idx <- newMVar =<< Index.newIndex
         uniq <- newIORef 0
         fn (Existing idx uniq [] 0)
withExisting m fn
    = fn m

findValue range [] = Nothing
findValue range ((uniqId, value):rs)
    | uniqId `isInRange` range = value
    | otherwise = findValue range rs


isInRange :: Int -> [Range] -> Bool
isInRange i [] = False
isInRange i (Range x y:rs)
    | i > x = False
    | i < y = isInRange i rs
    | otherwise = True

addToRange :: Int -> [Range] -> [Range]
addToRange i [] = [Range i i]
addToRange i (Range x y:rs)
    = merge (Range i i:Range x y:rs)

merge [] = []
merge [x] = [x]
merge (Range x y:Range a b:rs)
    | y == a+1    = merge (Range x b:rs)
    | otherwise = Range x y:merge (Range a b:rs)


