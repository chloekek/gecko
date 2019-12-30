{-# LANGUAGE ApplicativeDo #-}

module Gecko.As.Parse
  ( parseUnit
  ) where

import Gecko.As.Ast

import Control.Lens ((%~), _Left)
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor (void)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Void (Void)
import Gecko.C.Name (Identifier (..))
import Gecko.Position (Position (..))

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Text.Megaparsec as P

--------------------------------------------------------------------------------
-- Boilerplate

parseUnit :: String -> Text -> Either String Unit
parseUnit file input = P.parse unit file input & _Left %~ P.errorBundlePretty

type Parser = P.Parsec Void Text

position :: Parser Position
position = do
  sourcePos <- P.getSourcePos
  let file   = Text.pack $ P.sourceName   sourcePos
  let line   = P.unPos   $ P.sourceLine   sourcePos
  let column = P.unPos   $ P.sourceColumn sourcePos
  pure $ Position file line column

--------------------------------------------------------------------------------
-- Units

unit :: Parser Unit
unit = P.many definition

--------------------------------------------------------------------------------
-- Definitions

definition :: Parser Definition
definition = valueDefinition P.<|> wrapperDefinition

valueDefinition :: Parser Definition
valueDefinition = do
  π     <- position
  value <- keyword "value" *> identifier
  of_   <- specialOf       *> type1
  is    <- specialIs       *> term1
  _     <- keyword "end"
  pure $ ValueDefinition π value of_ is

wrapperDefinition :: Parser Definition
wrapperDefinition = do
  π       <- position
  wrapper <- keyword "wrapper" *> identifier
  of_     <- specialOf *> annotatedName kind1 `P.sepEndBy` punctuation ","
  is      <- specialIs *> type1
  _       <- keyword "end"
  pure $ WrapperDefinition π wrapper of_ is

--------------------------------------------------------------------------------
-- Kinds

kind1 :: Parser Kind
kind1 = do
  π <- position
  _ <- keyword "type"
  pure $ TypeKind π

--------------------------------------------------------------------------------
-- Types

type1 :: Parser Type
type1 = do
  π          <- position
  components <- type2 `P.sepBy1` punctuation "->"
  let makeFunctionType a b =
        ApplyType π (ApplyType π (NamedType π OperatorRightArrow) a) b
  pure $ foldr1 makeFunctionType components

type2 :: Parser Type
type2 = do
  π          <- position
  components <- P.some type3
  pure $ foldr1 (ApplyType π) components

type3 :: Parser Type
type3 = namedType P.<|> recordType P.<|> variantType P.<|> forallType
  where
  namedType :: Parser Type
  namedType = do
    π    <- position
    name <- identifier
    pure $ NamedType π name

  recordType :: Parser Type
  recordType = do
    π      <- position
    _      <- punctuation "{"
    fields <- annotatedName type1 `P.sepEndBy` punctuation "*"
    _      <- punctuation "}"
    let makeRowConsType (name, type_) rest = RowConsType π name type_ rest
    let rowType = foldr makeRowConsType (RowNilType π) fields
    pure $ ApplyType π (NamedType π "record") rowType

  variantType :: Parser Type
  variantType = do
    π        <- position
    _        <- punctuation "["
    variants <- annotatedName type1 `P.sepEndBy` punctuation "+"
    _        <- punctuation "]"
    let makeRowConsType (name, type_) rest = RowConsType π name type_ rest
    let rowType = foldr makeRowConsType (RowNilType π) variants
    pure $ ApplyType π (NamedType π "variant") rowType

  forallType :: Parser Type
  forallType = do
    π     <- position
    _     <- keyword "forall"
    names <- P.some identifier
    _     <- punctuation "=>"
    inner <- type1
    pure $ foldr (ForallType π) inner names

--------------------------------------------------------------------------------
-- Terms

term1 :: Parser Term
term1 =
  -- TODO: ApplyTerm
  term2

term2 :: Parser Term
term2 = parenthesizedTerm P.<|> variableTerm P.<|> lambdaTerm P.<|>
        recordTerm P.<|> injectTerm P.<|> wrapTerm
  where
  parenthesizedTerm :: Parser Term
  parenthesizedTerm = punctuation "(" *> term1 <* punctuation ")"

  variableTerm :: Parser Term
  variableTerm = do
    π <- position
    name <- identifier
    pure $ VariableTerm π name

  lambdaTerm :: Parser Term
  lambdaTerm = do
    π      <- position
    _      <- keyword "fun"
    params <- P.some identifier
    _      <- punctuation "=>"
    body   <- term1
    pure $ foldr (LambdaTerm π) body params

  recordTerm :: Parser Term
  recordTerm = do
    π      <- position
    _      <- punctuation "{"
    fields <- field `P.sepEndBy` punctuation ","
    _      <- punctuation "}"
    pure $ RecordTerm π fields
    where
      field = do
        name  <- identifier
        _     <- specialIs
        value <- term1
        pure (name, value)

  injectTerm :: Parser Term
  injectTerm = do
    π       <- position
    _       <- keyword "inject"
    variant <- identifier
    inner   <- term2
    pure $ InjectTerm π variant inner

  wrapTerm :: Parser Term
  wrapTerm = do
    π       <- position
    _       <- keyword "wrap"
    wrapper <- identifier
    inner   <- term2
    pure $ InjectTerm π wrapper inner

--------------------------------------------------------------------------------
-- Miscellaneous

annotatedName :: Parser p -> Parser (Identifier, p)
annotatedName p = do
   name <- identifier
   of_  <- specialOf *> p
   pure (name, of_)

--------------------------------------------------------------------------------
-- Terminals

terminal :: Parser a -> Parser a
terminal p = whitespaces *> p <* whitespaces
  where whitespaces = P.many whitespace
        whitespace  = P.oneOf [' ', '\n', '\r', '\t']

identifier :: Parser Identifier
identifier = terminal . P.try $ do
  name <- fmap Text.pack . P.some $ P.oneOf $ ['a' .. 'z'] ++ ['/']
  when (name `HashSet.member` reserved) $
    fail ("‘" ++ show name ++ "’ is a keyword")
  pure $ Identifier name

  where
  reserved :: HashSet Text
  reserved = HashSet.fromList
    [ "end"
    , "forall"
    , "fun"
    , "inject"
    , "is"
    , "of"
    , "value"
    , "wrap"
    , "wrapper" ]

keyword :: Text -> Parser ()
keyword w = terminal $ do
  _ <- P.chunk w
  P.notFollowedBy $ P.oneOf ['a' .. 'z']

punctuation :: Text -> Parser ()
punctuation = void . terminal . P.chunk

specialIs, specialOf :: Parser ()
specialIs = punctuation "=" P.<|> keyword "is"
specialOf = punctuation ":" P.<|> keyword "of"
