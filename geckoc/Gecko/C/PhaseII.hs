{-# LANGUAGE StrictData #-}

module Gecko.C.PhaseII
  ( PhaseII
  , Error (..)
  , TypeDefinition (..)
  , runPhaseII
  , execPhaseII
  , recordWrapperDefinition
  ) where

import Control.Lens (Lens', (%=), (?=), _Just,  at, isn't, lens, to, use, view)
import Control.Monad.RWS (RWS, runRWS)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Gecko.C.Expression (Expression (Lambda), Kind, Type)
import Gecko.C.Name (Global, Local)
import Gecko.C.TypeCheck (infer, runTypeCheck, unify)
import Gecko.Position (Position)

import qualified Data.HashMap.Strict as HashMap
import qualified Gecko.C.TypeCheck as TypeCheck

--------------------------------------------------------------------------------
-- Types

newtype PhaseII a =
  PhaseII { unPhaseII :: RWS (HashMap Global Kind) () Recording a }
  deriving newtype (Functor, Applicative, Monad)

data Recording =
  Recording
    { recordingTypeDefinitions :: HashMap Global TypeDefinition
    , recordingErrors :: [Error] }

data Error
  = MissingKindError Position Global
  | RedefinitionError Position Global
  | KindError (TypeCheck.Error 1)
  deriving stock (Show)

data TypeDefinition
  = WrapperDefinition Kind [Local] Type
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Running

runPhaseII :: PhaseII a -> HashMap Global Kind
           -> (a, Either (NonEmpty Error) (HashMap Global TypeDefinition))
runPhaseII (PhaseII action) kinds =
  let initialState = Recording HashMap.empty [] in
  let (returnValue, finalState, ()) = runRWS action kinds initialState in
  case recordingErrors finalState of
    []       -> (returnValue, Right (recordingTypeDefinitions finalState))
    (e : es) -> (returnValue, Left (e :| es))

execPhaseII :: PhaseII () -> HashMap Global Kind
            -> Either (NonEmpty Error) (HashMap Global TypeDefinition)
execPhaseII = (snd .) . runPhaseII

--------------------------------------------------------------------------------
-- Recording

recordWrapperDefinition :: Position -> Global -> [Local] -> Type -> PhaseII ()
recordWrapperDefinition π name params inner =
  PhaseII $ do
    isNew <- use $ _recordingTypeDefinitions . at name . to (isn't _Just)
    if isNew
      then view (at name) >>= \case
             Just kind -> do
               assertKind (foldr (Lambda π) inner params) kind & unPhaseII
               let typeDefinition = WrapperDefinition kind params inner
               _recordingTypeDefinitions . at name ?= typeDefinition
             Nothing -> _recordingErrors %= (MissingKindError π name :)
      else _recordingErrors %= (RedefinitionError π name :)

--------------------------------------------------------------------------------
-- Kind checking

assertKind :: Type -> Kind -> PhaseII ()
assertKind type_ expectedKind =
  PhaseII $ do
    kinds <- view id
    let environment = TypeCheck.Environment kinds HashMap.empty
    -- The expected kind does not need to be Skolemized
    -- because kind polymorphism is not implemented.
    let action = infer type_ >>= unify expectedKind
    case runTypeCheck action environment of
      Left kindErrors ->
        for_ kindErrors $ \kindError ->
          _recordingErrors %= (KindError kindError :)
      Right () -> pure ()

--------------------------------------------------------------------------------
-- Boilerplate

_recordingTypeDefinitions :: Lens' Recording (HashMap Global TypeDefinition)
_recordingTypeDefinitions = lens recordingTypeDefinitions (\s a -> s { recordingTypeDefinitions = a })

_recordingErrors :: Lens' Recording [Error]
_recordingErrors = lens recordingErrors (\s a -> s { recordingErrors = a })
