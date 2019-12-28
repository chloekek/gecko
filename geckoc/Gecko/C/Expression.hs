{-# LANGUAGE StrictData #-}

module Gecko.C.Expression
  ( Expression (..)
  , Term
  , Type
  , Kind
  ) where

import Data.Word (Word64)
import GHC.TypeLits (type (+), Nat)
import Gecko.C.Name (Global, Local)
import Gecko.Position (Position)

data Expression :: Nat -> * where
  Unknown :: Position -> Word64 -> Expression (u + 1)
  GetGlobal :: Position -> Global -> Expression u
  GetLocal :: Position -> Local -> Expression u
  Lambda :: Position -> Local -> Expression u -> Expression u
  Apply :: Position -> Expression u -> Expression u -> Expression u
deriving stock instance Show (Expression u)

type Term = Expression 0
type Type = Expression 1
type Kind = Expression 2
