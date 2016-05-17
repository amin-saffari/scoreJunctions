{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Parse.Junction where

import Data.Word
import Data.Attoparsec.ByteString.Char8
--import Data.Attoparsec.Text 
--import qualified Data.Text as B
--import qualified Data.Text.Char8 as B8
--import qualified Data.Text.Lazy as BL

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Data.Either (rights)

import Pragmatic.Types
import Pragmatic.Parse.Util
import Pragmatic.Junctions
import Pragmatic.Parse.Haarz

-- Parser for test-realignment format

-- After switching to Text instead of ByteString
--parseJunctions :: (Parser Junction) -> [BL.Text] -> Junctions
parseJunctions :: (Parser Junction) -> [BL.ByteString] -> Junctions
parseJunctions parser rawJunctions = rights $ map (parseJunction parser) rawJunctions

-- After switching to Text instead of ByteString
parseJunction :: (Parser Junction) -> BL.ByteString -> Either String Junction
--parseJunction :: (Parser Junction) -> BL.Text -> Either String Junction
parseJunction p junctionEntry = parseOnly p jE
    where
        jE = (B.concat . BL.toChunks) junctionEntry

-- |Parsing either "chr10 298965 299114 splits:1:1:1:C:P 0 +"
-- |or "chr10 298965 299114 splits:1:1:1:C:P 0 +"

parseAsHaarzJunction :: Parser Junction
parseAsHaarzJunction = fmap Junction
                  parseChrName 
              <*> (tab *> parsePosition)
              <*> (tab *> parsePosition)
              <*> (tab *> parseJunctionType)
              <*> (colon *> parseReadSupport)
              <*> (colon *> parseCountJunction)
              <*> (colon *> parseCountJunction)
              <*> (colon *> parseSpliceType)
              <*> (colon *> parseQualityFlag)
              <*> (tab *> parseScore)
              <*> (tab *> parseStrandSign)


----------------------------------
--- Probably nonSense till EOF ---
----------------------------------
--parseInfoField :: Parser InfoField
--parseInfoField = fmap InfoField
--                   (stringCI <* colon)
--               <*> (stringCI <* colon) -- maybe
--               <*> (decimal <* colon) --maybe
--               <*> (decimal <* colon)
--               <*> (decimal <* colon)
--               <*> (decimal <* colon)
--               <*> (char <* colon)
--               <*> (qualityFlage <- char)
--  return $ spliceType distantSpliceChr distantSplicePosition readsCount countJunctionsPos1 countJunctionsPos2 junctionType qualityFlage 



--parseInfoField :: Parser InfoField
--parseInfoField = do
--  spliceType <- stringCI <* colon
--  when (spliceType/="splice")
--  distantSpliceChr <- stringCI <* colon -- maybe
--  distantSplicePosition <- decimal <* colon --maybe
--  readsCount <- decimal <* colon
--  countJunctionsPos1 <- decimal <* colon
--  countJunctionsPos2 <- decimal <* colon
--  junctionType <- char <* colon
--  qualityFlage <- char
--  return $ spliceType distantSpliceChr distantSplicePosition readsCount countJunctionsPos1 countJunctionsPos2 junctionType qualityFlage 


