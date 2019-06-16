{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Options.Flags
  ( Flags
  , IsFlag
  , flag
  , enabled
  , disabled
  , setFlag
  , unsetFlag
  ) where

import qualified Data.Bits as B
import Data.List (sort)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import GHC.Exts
import GHC.Prim (Proxy#, proxy#)

newtype Flags a = Flags (V.Vector Word64) deriving Eq

type IsFlag a = (Bounded a, Enum a, Ord a)

size :: forall a. IsFlag a => Proxy# a -> Int
size _ = fromEnum (maxBound @a) `quot` 64 + 1

-- (elem in vector, bit index)
index :: IsFlag a => a -> (Int, Int)
index a = quotRem (fromEnum a) 64

instance (Show a, IsFlag a) => Show (Flags a) where
  show = show . toList

instance IsFlag a => IsList (Flags a) where
  type Item (Flags a) = a

  -- FIXME: this is buggy
  fromList = Flags . V.unfoldr go . (0,) . sort
    where
      siz = size (proxy# :: Proxy# a)

      go (!vi, []) | vi < siz = Just (0, (vi+1, []))
                   | otherwise = Nothing
      go (!vi, fs) = fill (vi, 0) fs

      fill (!v, !b)  [] = Just (b, (v, []))
      fill (!v, !b) (f:fs)
         | vi > v = Just (b, (vi, f:fs))
         | otherwise = fill (v, B.setBit b bi) fs
        where
          (vi, bi) = index f

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

flag :: IsFlag a => a -> Flags a
flag = flip setFlag mempty

enabled :: IsFlag a => a -> Flags a -> Bool
enabled a (Flags fs) = B.testBit (V.unsafeIndex fs vi) bi
  where (vi,bi) = index a

disabled :: IsFlag a => a -> Flags a -> Bool
disabled a fs = not (enabled a fs)

setFlag :: IsFlag a => a -> Flags a -> Flags a
setFlag a (Flags fs) = Flags $ V.imap (\i f -> if i == vi then B.setBit f bi else f) fs
  where (vi,bi) = index a

unsetFlag :: IsFlag a => a -> Flags a -> Flags a
unsetFlag a (Flags fs) = Flags $ V.imap (\i f -> if i == vi then B.clearBit f bi else f) fs
  where (vi,bi) = index a

toggleFlag :: IsFlag a => a -> Flags a -> Flags a
toggleFlag a (Flags fs) = Flags $ V.imap (\i f -> if i == vi then B.complementBit f bi else f) fs
  where (vi,bi) = index a
