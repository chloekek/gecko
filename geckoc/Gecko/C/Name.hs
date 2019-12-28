module Gecko.C.Name
  ( Identifier (..)
  , Global (..)
  , Local (..)
  ) where

import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Identifier
  = Identifier Text
  | OperatorRightArrow
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable)
instance IsString Identifier where fromString = Identifier . fromString

newtype Global =
  Global Identifier
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString)

newtype Local =
  Local Identifier
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, IsString)
