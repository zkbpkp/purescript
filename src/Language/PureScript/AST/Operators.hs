{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Operators fixity and associativity
--
module Language.PureScript.AST.Operators where

import Prelude.Compat

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Data (Data)

import Language.PureScript.Crash

-- |
-- A precedence level for an infix operator
--
type Precedence = Integer

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr | Infix
  deriving (Data, Show, Eq, Ord, Generic)

instance NFData Associativity

showAssoc :: Associativity -> String
showAssoc Infixl = "infixl"
showAssoc Infixr = "infixr"
showAssoc Infix  = "infix"

readAssoc :: String -> Associativity
readAssoc "infixl" = Infixl
readAssoc "infixr" = Infixr
readAssoc "infix"  = Infix
readAssoc _ = internalError "readAssoc: no parse"

instance A.ToJSON Associativity where
  toJSON = A.toJSON . showAssoc

instance A.FromJSON Associativity where
  parseJSON = fmap readAssoc . A.parseJSON

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity Associativity Precedence
  deriving (Data, Show, Eq, Ord, Generic)

instance NFData Fixity

instance A.ToJSON Fixity where
  toJSON (Fixity associativity precedence) =
    A.object [ "associativity" .= associativity
             , "precedence" .= precedence
             ]
