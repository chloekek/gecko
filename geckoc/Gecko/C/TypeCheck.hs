{-# LANGUAGE StrictData #-}

module Gecko.C.TypeCheck
  ( TypeCheck (..)
  , Environment (..)
  , Error (..)
  , runTypeCheck
  , infer
  , unify
  ) where

import Control.Lens (Lens', (?~), (%=), (?=), (<<+=), at, lens, use, view)
import Control.Monad (join)
import Control.Monad.RWS (RWS, runRWS)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64)
import GHC.TypeLits (type (+))
import Gecko.C.Expression (Expression (..))
import Gecko.C.Name (Global (..), Identifier (..), Local)
import Gecko.Position (Position)

import qualified Control.Monad.RWS as RWS
import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------
-- Types

newtype TypeCheck u a =
  TypeCheck { unTypeCheck :: RWS (Environment u) () (Unification u) a }
  deriving newtype (Functor, Applicative, Monad)

data Environment u =
  Environment
    { environmentGlobals :: HashMap Global (Expression (u + 1))
    , environmentLocals :: HashMap Local (Expression (u + 1)) }

data Unification u =
  Unification
    { unificationNextFresh :: Word64
    , unificationSolutions :: HashMap Word64 (Expression (u + 1))
    , unificationErrors :: [Error u] }

data Error u
  = UnknownGlobal Position Global
  | UnknownLocal Position Local
  | UnificationError (Expression (u + 1)) (Expression (u + 1))
  deriving stock (Show)

local :: (Environment u -> Environment u) -> TypeCheck u a -> TypeCheck u a
local f (TypeCheck action) = TypeCheck $ RWS.local f action

--------------------------------------------------------------------------------
-- Running

runTypeCheck :: TypeCheck u a -> Environment u -> Either (NonEmpty (Error u)) a
runTypeCheck (TypeCheck action) environment =
  let initialState = Unification 0 HashMap.empty [] in
  let (returnValue, finalState, ()) = runRWS action environment initialState in
  case unificationErrors finalState of
    []       -> Right returnValue
    (e : es) -> Left (e :| es)

--------------------------------------------------------------------------------
-- Inference

freshUnknown :: Position -> TypeCheck u (Expression (u + 1))
freshUnknown π = TypeCheck $ fmap (Unknown π) $ _unificationNextFresh <<+= 1

infer :: Expression u -> TypeCheck u (Expression (u + 1))

infer Unknown{} =
  error "Not sure how to infer type of of unknown???"

infer (GetGlobal π global) =
  TypeCheck $
    view (_environmentGlobals . at global) >>= \case
      Just type_ -> pure type_
      Nothing    -> do _unificationErrors %= (UnknownGlobal π global :)
                       freshUnknown π & unTypeCheck

infer (GetLocal π locαl) =
  TypeCheck $
    view (_environmentLocals . at locαl) >>= \case
      Just type_ -> pure type_
      Nothing    -> do _unificationErrors %= (UnknownLocal π locαl :)
                       freshUnknown π & unTypeCheck

infer (Lambda π param body) = do
  paramType <- freshUnknown π
  bodyType  <- local (_environmentLocals . at param ?~ paramType) $
                 infer body

  let functionType = GetGlobal π (Global OperatorRightArrow)
  pure $ Apply π (Apply π functionType paramType) bodyType

infer (Apply π func arg) = do
  funcType <- infer func
  argType  <- infer arg

  let functionType = GetGlobal π (Global OperatorRightArrow)
  returnType <- freshUnknown π
  unify funcType (Apply π (Apply π functionType argType) returnType)

  pure returnType

--------------------------------------------------------------------------------
-- Unification

prune :: Expression (u + 1) -> TypeCheck u (Expression (u + 1))
prune (Unknown π unknown) =
  TypeCheck $ do
    use (_unificationSolutions . at unknown) >>= \case
      Just solution -> prune solution & unTypeCheck
      Nothing       -> pure $ Unknown π unknown
prune other = pure other

unify :: Expression (u + 1) -> Expression (u + 1) -> TypeCheck u ()
unify = \left right -> join $ go <$> prune left <*> prune right
  where
  solve :: Word64 -> Expression (u + 1) -> TypeCheck u ()
  solve unknown type_ = TypeCheck $ _unificationSolutions . at unknown ?= type_

  failure :: Expression (u + 1) -> Expression (u + 1) -> TypeCheck u ()
  failure a b = TypeCheck $ _unificationErrors %= (UnificationError a b :)

  go :: Expression (u + 1) -> Expression (u + 1) -> TypeCheck u ()

  go (Unknown _ a) (Unknown _ b) | a == b = pure ()
  go (Unknown _ a) b = solve a b
  go a (Unknown _ b) = solve b a

  go (GetGlobal _ a) (GetGlobal _ b) | a == b = pure ()
  go a@GetGlobal{} b = failure a b

  go (GetLocal _ a) (GetLocal _ b) | a == b = pure ()
  go a@GetLocal{} b = failure a b

  -- TODO: This should do alpha equivalence.
  go (Lambda _ ap ab) (Lambda _ bp bb) | ap == bp = unify ab bb
  go a@Lambda{} b = failure a b

  go (Apply _ af ax) (Apply _ bf bx) = do
    unify af bf
    unify ax bx
  go a@Apply{} b = failure a b

--------------------------------------------------------------------------------
-- Boilerplate

_environmentGlobals :: Lens' (Environment u) (HashMap Global (Expression (u + 1)))
_environmentGlobals = lens environmentGlobals (\s a -> s { environmentGlobals = a })

_environmentLocals :: Lens' (Environment u) (HashMap Local (Expression (u + 1)))
_environmentLocals = lens environmentLocals (\s a -> s { environmentLocals = a })

_unificationNextFresh :: Lens' (Unification u) Word64
_unificationNextFresh = lens unificationNextFresh (\s a -> s { unificationNextFresh = a })

_unificationSolutions :: Lens' (Unification u) (HashMap Word64 (Expression (u + 1)))
_unificationSolutions = lens unificationSolutions (\s a -> s { unificationSolutions = a })

_unificationErrors :: Lens' (Unification u) [Error u]
_unificationErrors = lens unificationErrors (\s a -> s { unificationErrors = a })
