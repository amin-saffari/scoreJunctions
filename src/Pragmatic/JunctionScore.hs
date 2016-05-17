{-# LANGUAGE DeriveGeneric #-}

module Pragmatic.JunctionScore where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.Text
import Data.Text.Lazy
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import Data.Monoid

import Pragmatic.Types
import Pragmatic.Junctions


data JunctionScore = JunctionScore {
      junctionReadCount   :: ReadSupport
    , junctionTypeDef     :: JunctionType
    , junctionQualityFlag :: QualityFlag
    , junctionSpliceType  :: SpliceType
    , junctionChrName     :: ChrName
    , junctionPosition    :: Position
    , junctionScore       :: Score
    , junctionStrand      :: StrandSign
    } deriving (Show, Generic)

toJunctionScoreLine site = JunctionScore _junctionReadCount _junctionType _junctionQualityFlag
                                     _junctionSpliceType _junctionChrName _junctionPosition
                                     _junctionScore _junctionStrand
     where
         _junctionReadCount    = readsCount site 
         _junctionType         = junctionType site -- It's with its type constract
         _junctionQualityFlag  = qualityFlag site
         _junctionSpliceType   = spliceType site
         _junctionChrName      = chrName site
         _junctionPosition     = positionOne site
         _junctionScore        = score site
         _junctionStrand       = strand site

toJunctionScores::Junctions  -> [BL.ByteString]
toJunctionScores js = [ toJunctionScore j | j<- js]

toJunctionScore::Junction -> BL.ByteString
toJunctionScore j = TL.encodeUtf8 . B.toLazyText $ 
         B.decimal (readsCount j) <> B.singleton '\t' <>
         (case junctionType j of
            Splits -> B.fromString "Splits" 
            Strandsplice -> B.fromString "Strandsplice"
            Distsplice chr pos -> B.fromString "Distsplice\t" <> (B.fromText . T.decodeUtf8) chr -- <>  B.decimal pos
         ) <> B.singleton '\t' <>
         (case qualityFlag j of
           F -> B.fromString "F"
           P -> B.fromString "P"
           M -> B.fromString "M"
         ) <> B.singleton '\t' <>
         (case spliceType j of
           A -> B.fromString "A"
           D -> B.fromString "D"
           R -> B.fromString "R"
           L -> B.fromString "L"
           C -> B.fromString "C"
           N -> B.fromString "N"
         ) <> B.singleton '\t' <>
         (B.fromText . T.decodeUtf8) (chrName j) <> B.singleton '\t' <>
         B.decimal (positionOne j) <> B.singleton '\t' <>
         B.realFloat (score j) <> B.singleton '\t' <>
         (case strand j of
           Forward -> B.fromString "Forward"
           Reverse -> B.fromString "Reverse"
           Unknown -> B.fromString "Unknown"
         ) <> B.singleton '\t'

-- <> is synonymous to mappend 




--        strand j  
--         fromText (chrName j) <> singleton ',' <>
--         decimal (positionOne j) <> singleton ',' <>
--         decimal (positionTwo j) <> singleton ',' <>
--         (case junctionType j of 
--             Strandsplice -> mempty
--             Distsplice chr pos -> fromText chr <> singleton ',' <> decimal pos)

--encode . (map toJunctioinScore)

-- In general:
-- (f . g) x = f (g x) and f $ x = f x
-- $ is for avoiding parentheses. Anything appearing after it will take precedence over anything that comes before
-- e.g: (show (1+1)) == (show $ 1 + 1) == $ show (1+1) == $ show $ 1+1
-- . is for chain functions. it lets you tie the output of whatever appears on the right to the input of whatever appears 
-- on the left. This usually also results in fewer parentheses, but works differently.
-- So (1+ 1) doesn't have an input, and therefore cannot be used with the . operator
-- show can take an Int and return a string putStrLn also can take a string and return a IO()
-- so you can chain show to putStrLn like (putStrLn . show) (1+1) or putStrLn . show $ 1+1 or even more crazier
-- putStrLn . show . (+) 1 1
