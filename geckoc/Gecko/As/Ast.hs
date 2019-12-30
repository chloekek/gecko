{-# LANGUAGE StrictData #-}

module Gecko.As.Ast
  ( Unit
  , Definition (..)
  , Kind (..)
  , Type (..)
  , Term (..)
  ) where

import Gecko.C.Name (Identifier)
import Gecko.Position (Position)

type Unit = [Definition]

data Definition
  = ValueDefinition Position Identifier Type Term
  | WrapperDefinition Position Identifier [(Identifier, Kind)] Type
  deriving stock (Show)

data Kind
  = TypeKind Position
  deriving stock (Show)

data Type
  = NamedType Position Identifier
  | ApplyType Position Type Type
  | RowNilType Position
  | RowConsType Position Identifier Type Type
  | ForallType Position Identifier Type
  deriving stock (Show)

data Term
  = VariableTerm Position Identifier
  | LambdaTerm Position Identifier Term
  | RecordTerm Position [(Identifier, Term)]
  | InjectTerm Position Identifier Term
  | WrapTerm Position Identifier Term
  deriving stock (Show)
