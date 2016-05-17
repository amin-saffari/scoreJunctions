{-# LANGUAGE DeriveGeneric #-}

module Pragmatic.GTF where

import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy as BL 
import GHC.Generics


-- | GTFrecord type of each JSON entry in record syntax.
data GTFrecord = GTFrecord {
    seqname    :: String -- w/o chr in Ensemble standard.
  , source     :: String -- datasource/Project name.
  , feature    :: String --Feature -- Gene, Variation, Similarity.
  , start      :: Int -- position.
  , end        :: Int --position.
  , score      :: String -- floating point value / . if there is no information.
  , transcriptStrand     :: Char -- +/-
  , frame      :: String -- 0,1,2/. if there is no information.
  , attributes :: [Attribute] --pairs of additional information about each feature.
  } deriving (Show,Generic)

-- | Attribute type of each JSON entry in record syntax.
data Attribute = Attribute { 
    name :: String
  , value :: String
  } deriving (Show, Generic)


-- | Valid values of feature field
--data  Feature = Gene | Transcript | Exon deriving Show
-- rightnow just a simple string but definatly I should change it 


-- | , childs :: [GTFrecord] Not sure about this but I want somethings which 
-- |was not resambling plain text so for wach gene 

-- | I must have all of it's transcript in another level and then on deeper level 
-- | all exons on the same transcript!


-- | Generic: Haskell has a language extension called DeriveGeneric. Using this 
-- | extension will allow you to tell Haskell to write the instance for you

--  , estCount      :: Int    -- |How many ESTs refere to this SS (If there is no EST 
                            -- |source, then the count refers to RefSeq or Gencode 
                            -- |Data, if not O)
--9	chr1	-	172689	S	A	4	undef,	chr1:173752:-,	?	brain_normal:4,	cagacccttgtctccttctagGCCCTCACAATCCAGTGGAG	yes
