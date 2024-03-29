{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Text.Short.Unlifted
  ( ShortText# (..)
  , lift
  , unlift
  ) where

import Data.ByteString.Short.Internal as TS
import Data.Text.Short (ShortText)
import Data.Unlifted (ShortText# (ShortText#))

import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS

lift :: ShortText# -> ShortText
lift (ShortText# x) = TS.fromShortByteStringUnsafe (SBS x)

unlift :: ShortText -> ShortText#
unlift t = case TS.toShortByteString t of
  SBS x -> ShortText# x
