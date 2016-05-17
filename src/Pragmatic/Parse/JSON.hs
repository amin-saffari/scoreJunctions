{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Parse.JSON where

import Data.Aeson 
import Pragmatic.Types
-- Paser import
import Data.Word
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

import Pragmatic.GTF

-- JSON generic function
instance FromJSON GTFrecord
instance ToJSON GTFrecord

-------------------------------------------------------------------------------
instance FromJSON Attribute
instance ToJSON Attribute

-------------------------------------------------------------------------------

