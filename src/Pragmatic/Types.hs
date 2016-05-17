{-# LANGUAGE DeriveGeneric #-}

module Pragmatic.Types where

import Data.ByteString.Char8 as BC (ByteString)
import Data.Text

-- |Type synonym
type Position = Int
type Score = Double
type ChrName = BC.ByteString 
type ReadSupport = Int
type CountJunction = Int

-- |Possible SpliceType, QualityFlag, StrandSign and JunctionType
data SpliceType = A | D | R | L | C | N deriving (Show, Eq)
data QualityFlag = P | M | F deriving (Show, Eq)
data StrandSign = Forward | Reverse | Unknown deriving (Show, Eq)

data JunctionType = Splits 
                  | Strandsplice
                  | Distsplice { distantSpliceChr      :: ChrName
                               , distantSplicePosition :: Position 
                               } deriving (Show, Eq)


--Might be an good example
--  , estCount      :: Int    -- |How many ESTs refere to this SS (If there is no EST 
                            -- |source, then the count refers to RefSeq or Gencode 
                            -- |Data, if not O)
--9	chr1	-	172689	S	A	4	undef,	chr1:173752:-,	?	brain_normal:4,	cagacccttgtctccttctagGCCCTCACAATCCAGTGGAG	yes
