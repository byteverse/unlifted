{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language GADTSyntax #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language PolyKinds #-}
{-# language TypeApplications #-}
{-# language TypeInType #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module Data.Unlifted
  ( -- * Base
    Maybe#(..)
  , Either#(..)
  , ST#(..)
    -- * Text
  , ShortText#(..)
    -- * Arrays
  , PrimArray#(..)
  , MutablePrimArray#(..)
    -- * Boolean
  , Bool#(..)
  , pattern True#
  , pattern False#
  ) where

import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Levity(Unlifted),RuntimeRep(..),Int#,ByteArray#,MutableByteArray#)

-- | Variant of @ST@ where the argument type does not have to be lifted.
-- This does not have a monad instance and is difficult to use.
newtype ST# :: forall (r :: RuntimeRep). Type -> TYPE r -> Type where
  ST# :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r).
    { unST# :: State# s -> (# State# s, a #)
    } -> ST# s a

-- | Unboxed variant of @Bool@. This might be changed to use @Int8Rep@ in the
-- future.
newtype Bool# :: TYPE 'IntRep where
  Bool# :: Int# -> Bool#

-- | Unboxed variant of @Maybe@.
newtype Maybe# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('SumRep '[ 'TupleRep '[], r ]) where
  Maybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# (# #) | a #) -> Maybe# @r a

-- | Unboxed variant of @Either@.
newtype Either# :: forall (ra :: RuntimeRep) (rb :: RuntimeRep). TYPE ra -> TYPE rb -> TYPE ('SumRep '[ ra, rb ]) where
  Either# :: forall (ra :: RuntimeRep) (rb :: RuntimeRep) (a :: TYPE ra) (b :: TYPE rb). (# a | b #) -> Either# a b

{-# complete True#, False# #-}

pattern True# :: Bool#
pattern True# = Bool# 1#

pattern False# :: Bool#
pattern False# = Bool# 0#

-- | Mutable variant of 'PrimArray#'.
newtype MutablePrimArray# :: forall (r :: RuntimeRep). Type -> TYPE r -> TYPE ('BoxedRep 'Unlifted) where
  MutablePrimArray# :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutableByteArray# s -> MutablePrimArray# s a

-- | This resembles the @PrimArray@ type from @primitive@, but the phantom
-- parameter is an unboxed type, not a lifted type. For example:
--
-- * @PrimArray Word8@
-- * @PrimArray# Word8#@
newtype PrimArray# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('BoxedRep 'Unlifted) where
  PrimArray# :: forall (r :: RuntimeRep) (a :: TYPE r). ByteArray# -> PrimArray# a

-- | Unlifted variant of @ShortText@.
newtype ShortText# :: TYPE ('BoxedRep 'Unlifted) where
  ShortText# :: ByteArray# -> ShortText#
