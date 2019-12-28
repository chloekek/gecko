{-# LANGUAGE StrictData #-}

module Gecko.Position
  ( Position (..)
  ) where

import Data.Text (Text)

data Position =
  Position
    { positionFile   :: Text
    , positionLine   :: {-# UNPACK #-} Int
    , positionColumn :: {-# UNPACK #-} Int }
  deriving stock (Show)
