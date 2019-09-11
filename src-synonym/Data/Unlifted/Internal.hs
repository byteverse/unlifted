{-# language TypeInType #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language ExplicitForAll #-}
{-# language PatternSynonyms #-}

module Data.Unlifted.Internal
  ( Bool#
  , Maybe#
  , Either#
    -- * Bool Patterns
  , pattern True#
  , pattern False#
  ) where

import Data.Kind (Type)
import GHC.Exts (TYPE,State#,RuntimeRep(..),Int#)

type Bool# = Int#
type Maybe# (a :: TYPE ra) = (# (# #) | a #)
type Either# (a :: TYPE ra) (b :: TYPE rb) = (# a | b #)

{-# complete True#, False# #-}

pattern True# :: Bool#
pattern True# = 1#

pattern False# :: Bool#
pattern False# = 0#

-- pattern Nothing# :: forall (r :: RuntimeRep) (a :: TYPE r). a -> (# (# #) | a #)
-- pattern Nothing# = (# (# #) | #)

