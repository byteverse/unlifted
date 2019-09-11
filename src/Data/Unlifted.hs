{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language UnboxedTuples #-}
{-# language ExplicitForAll #-}
{-# language GADTSyntax #-}
{-# language DataKinds #-}
{-# language TypeInType #-}

module Data.Unlifted
  ( Bool#
  , Maybe#
  , Either#
  , ST#(..)
    -- * Bool Patterns
  , pattern True#
  , pattern False#
  ) where

import Data.Unlifted.Internal (Maybe#,Either#,Bool#,pattern True#,pattern False#)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,RuntimeRep)

newtype ST# :: forall (r :: RuntimeRep). Type -> TYPE r -> Type where
  ST# :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r).
    { unST# :: State# s -> (# State# s, a #)
    } -> ST# s a


