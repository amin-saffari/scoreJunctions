{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Parse.Util (tab,colon) where

import Data.Attoparsec.ByteString.Char8

import Pragmatic.Types

tab :: Parser Char
tab = char '\t'

colon :: Parser Char
colon = char ':'

