{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options.Flags
  ( Flags
  , enabled
  , disabled
  , setFlag
  , unsetFlag
  ) where

import qualified Data.Bits as B
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import GHC.Prim (Proxy#, proxy#)

newtype Flags a = Flags (V.Vector Word64) deriving Eq

type IsFlag a = (Bounded a, Enum a)

size :: forall a. IsFlag a => Proxy# a -> Int
size _ = fromEnum (maxBound @a) `quot` 64 + 1

-- (elem in vector, bit index)
toIndex :: IsFlag a => a -> (Int, Int)
toIndex a = quotRem (fromEnum a) 64

instance (Show a, Bounded a, Enum a) => Show (Flags a) where
  show = show . toList

toList :: IsFlag a => Flags a -> [a]
toList (Flags fs) = V.ifoldr go [] fs
  where
    go vi f acc = go2 0 acc
      where
        go2 !b acc
          | b == 64       = acc
          | B.testBit f b = go2 (b+1) (toEnum (vi+b) : acc)
          | otherwise     = go2 (b+1) acc

instance Semigroup (Flags a) where
  Flags f1 <> Flags f2 = Flags $ V.zipWith (B..|.) f1 f2

instance IsFlag a => Monoid (Flags a) where
  mempty = Flags $ V.replicate (size (proxy# :: Proxy# a)) 0

enabled :: IsFlag a => a -> Flags a -> Bool
enabled a (Flags fs) = B.testBit (V.unsafeIndex fs vi) bi
  where (vi,bi) = toIndex a

disabled :: IsFlag a => a -> Flags a -> Bool
disabled a fs = not (enabled a fs)

flag :: IsFlag a => a -> Flags a
flag = flip setFlag mempty

setFlag :: IsFlag a => a -> Flags a -> Flags a
setFlag a (Flags fs) = Flags $ V.imap (\i f -> if i == vi then B.setBit f bi else f) fs
  where (vi,bi) = toIndex a

unsetFlag :: IsFlag a => a -> Flags a -> Flags a
unsetFlag a (Flags fs) = Flags $ V.imap (\i f -> if i == vi then B.clearBit f bi else f) fs
  where (vi,bi) = toIndex a

toggleFlag :: IsFlag a => a -> Flags a -> Flags a
toggleFlag a (Flags fs) = Flags $ V.imap (\i f -> if i == vi then B.complementBit f bi else f) fs
  where (vi,bi) = toIndex a
