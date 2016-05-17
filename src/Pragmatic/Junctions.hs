{-# LANGUAGE DeriveGeneric #-}

module Pragmatic.Junctions where

import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy as BL 
import GHC.Generics

import Pragmatic.Types

--data JunctionType = Splits 
--              | Strandsplice
-- Not now              | Diststrandsplice
--              | Distsplice { distantSpliceChr      :: BC.ByteString
--                           , distantSplicePosition :: Int
--                            } deriving (Show, Eq)

-- Before I was thinking it's good to have this as a seperate record
-- | e.g: splits:365:380:370:N:P or distsplice:chr19:47506458:1:1:1:L:P
-- | e.g: chr10 298965 299114 splits:1:1:1:C:P 0 +
data Junction = Junction {
    chrName               :: ChrName 
  , positionOne           :: Position
  , positionTwo           :: Position
  , junctionType          :: JunctionType -- If it is Distsplice then the next two fields are not mine!
  , readsCount            :: ReadSupport 
  , countJunctionsPos1    :: CountJunction 
  , countJunctionsPos2    :: CountJunction
  , spliceType            :: SpliceType 
  , qualityFlag           :: QualityFlag 
  , score                 :: Score
  , strand                :: StrandSign
} deriving (Show,Generic)

-- | Whole file is lines of Junction records
type Junctions = [Junction]



--data JunctionInfoField = JunctionInfoField {
--    junctionType          :: JunctionType -- If it is Distsplice then the next two fields are not mine!
--  , readsCount            :: ReadSupport 
--  , countJunctionsPos1    :: Int
--  , countJunctionsPos2    :: Int
--  , spliceType            :: SpliceType  -- A | L | C | N
--  , qualityFlage          :: Flag -- Quality flage of this splice junction (either P,M,F)
--  } deriving (Show,Generic)

-- | filled with all fields but info (exception in case if Trans splicing which I do omit it now)
-- | Deciphering haarz info field
-- | e.g: splits:365:380:370:N:P or distsplice:chr19:47506458:1:1:1:L:P
--data SpliceSite = SpliceSite {
--    readsCount     :: Int    -- How many reads are supporting this splice site (1st Int)
--  , junctionType   :: Char   -- Type of a regular splice junction (1st Char e.g: Circular, Normal)
--  , qualityFlage   :: Char   -- Quality flage of this splice junction (last Char e.g: P,M,F)
--  , tSpliceType     :: String -- Cis or Trans splice (1st filed: Splits, Distsplice, Strandsplice, Diststrandsplice)
--  , sites          :: [Site] -- Information of this site
--  } deriving (Show,Generic)

--  , estCount      :: Int    -- |How many ESTs refere to this SS (If there is no EST 
                            -- |source, then the count refers to RefSeq or Gencode 
                            -- |Data, if not O)
--9	chr1	-	172689	S	A	4	undef,	chr1:173752:-,	?	brain_normal:4,	cagacccttgtctccttctagGCCCTCACAATCCAGTGGAG	yes
