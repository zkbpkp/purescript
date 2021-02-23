{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments where

import Prelude.Compat
import Data.Text (Text)
import Data.Data (Data)

import Data.Aeson.TH

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Data, Show, Eq, Ord)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
