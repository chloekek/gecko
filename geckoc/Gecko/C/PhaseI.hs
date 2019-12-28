{-# LANGUAGE StrictData #-}

module Gecko.C.PhaseI
  ( PhaseI
  , Error (..)
  , runPhaseI
  , execPhaseI
  , recordTypeDefinition
  ) where

import Control.Lens (Lens', (%=), (?=), _Just, at, isn't, lens, to, use)
import Control.Monad.State (State, runState)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Gecko.C.Expression (Kind)
import Gecko.C.Name (Global)
import Gecko.Position (Position)

import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------
-- Types

newtype PhaseI a =
  PhaseI (State Recording a)
  deriving newtype (Functor, Applicative, Monad)

data Recording =
  Recording
    { recordingKinds :: HashMap Global Kind
    , recordingErrors :: [Error] }

data Error
  = RedefinitionError Position Global
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Running

runPhaseI :: PhaseI a -> (a, Either (NonEmpty Error) (HashMap Global Kind))
runPhaseI (PhaseI action) =
  let initialState = Recording HashMap.empty [] in
  let (returnValue, finalState) = runState action initialState in
  case recordingErrors finalState of
    []       -> (returnValue, Right (recordingKinds finalState))
    (e : es) -> (returnValue, Left (e :| es))

execPhaseI :: PhaseI () -> Either (NonEmpty Error) (HashMap Global Kind)
execPhaseI = snd . runPhaseI

--------------------------------------------------------------------------------
-- Recording

recordTypeDefinition :: Position -> Global -> Kind -> PhaseI ()
recordTypeDefinition position name kind =
  PhaseI $ do
    isNew <- use $ _recordingKinds . at name . to (isn't _Just)
    if isNew then _recordingKinds . at name ?= kind
             else _recordingErrors %= (RedefinitionError position name :)

--------------------------------------------------------------------------------
-- Boilerplate

_recordingKinds :: Lens' Recording (HashMap Global Kind)
_recordingKinds = lens recordingKinds (\s a -> s { recordingKinds = a })

_recordingErrors :: Lens' Recording [Error]
_recordingErrors = lens recordingErrors (\s a -> s { recordingErrors = a })
