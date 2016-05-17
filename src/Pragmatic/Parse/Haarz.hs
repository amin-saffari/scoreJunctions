{-# LANGUAGE OverloadedStrings #-}

module Pragmatic.Parse.Haarz (
              parseChrName, 
              parsePosition, 
              parseJunctionType, 
              parseReadSupport, 
              parseCountJunction, 
              parseSpliceType, 
              parseQualityFlag, 
              parseScore, 
              parseStrandSign, 
              swapSide
       ) where


--import Prelude hiding (Data.Attoparsec.ByteString.Char8.D) 
--import Pragmatic.Types ( SpliceType(..), QualityFlag(..), StrandSign(..), ChrName(..), Position(..), JunctionType(..),ReadSupport(..), CountJunction(..), Score(..))

--import Data.Attoparsec.Text
--import Data.Attoparsec.Text.Parser
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import Prelude hiding (takeWhile)
--import Data.Word
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

import qualified Pragmatic.Types as TYS
import Pragmatic.Parse.Util

-- Parser for test-realignment format

parseChrName :: Parser TYS.ChrName
parseChrName =  takeTill (== '\t') 

parsePosition :: Parser TYS.Position
parsePosition = decimal

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$) :: Functor f => b -> f a -> f b
-- 
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (<*) :: Applicative f => f b -> f a -> f b
-- (*>) :: Applicative f => f a -> f b -> f b
-- 
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- liftM :: Monad f => (a -> b) -> f a -> f b
-- 
-- class Functor f => Applicative f where ...
-- class Applicative f => Monad f where
-- 
-- <$>
-- <*>

parseJunctionType :: Parser TYS.JunctionType
parseJunctionType = 
                   (TYS.Splits <$  stringCI "splits")
               <|> (TYS.Strandsplice <$ stringCI "strandsplice")
               <|> (TYS.Distsplice <$ string "distsplice" <* char ':'
                                   <*> takeWhile (/= ':')
                                   <*  char ':'
                                   <*> parsePosition)

--                TYS.Distsplice <$ string "distsplice" <* char ':'
--                                    <*> takeWhile (/= ':')
--                                    <*  char ':'
--                                    <*> parsePosition) :: Parser (Splice) 

--parseJunctionType :: Parser TYS.JunctionType
--parseJunctionType = TYS.Splits <$> string "splits"


-- Some synonym:
-- <$> = fmap
-- <*> = liftM
-- *> = >>
-- <*
-- <|>
-- *> applies its first argument, throws always its result, then applies the second and return its result.
-- more generally if there's an angle bracket pointing to some side, the result from that side should be used.
-- (*>) returns the result on its right; (<*>) returns results from both sides; and (<*),returns the result on its left.

-- data BLAHVALUE = B bool | ....
-- blahBlah :: Parser BLAHVALUE
-- | These are equivalant
-- blahBlah = B <$> bool
-- blahBlah = fmap B bool


--parseJunctionType :: Parser TYS.JunctionType
--parseJunctionType = do 
--                (stringCI "splits" *> return (TYS.Splits))
--            <|> (stringCI "distsplice" 
--                 <*> (chrname <- parseChrName *> semicolon)
--                 <*> (position<- parsePosition *> semicolon)
--                return (TYS.Distsplice chrName position)) -- I should say something about those 2 additional fields! But How!?
--            <|> fail "Invalid Junction Type"

--parseProtocolVersion :: Parser ProtocolVersion
--parseProtocolVersion = junctionType <$> (liftA2 (,) decimal (char '.' *> decimal))
--            where junctionType (mA, mN) = JunctionType mA mN


parseReadSupport :: Parser TYS.ReadSupport
parseReadSupport = decimal 

parseCountJunction :: Parser TYS.CountJunction
parseCountJunction = decimal

parseSpliceType :: Parser TYS.SpliceType
parseSpliceType = 
                (char 'A' *> return TYS.A)
            <|> (char 'D' *> return TYS.D)
            <|> (char 'C' *> return TYS.C)
            <|> (char 'R' *> return TYS.R)
            <|> (char 'L' *> return TYS.L)
            <|> (char 'N' *> return TYS.N)
            <|> fail "Invalid SpliceType"

parseQualityFlag :: Parser TYS.QualityFlag
parseQualityFlag = 
                (char 'P' *> return TYS.P)
            <|> (char 'M' *> return TYS.M)
            <|> (char 'F' *> return TYS.F)
            <|> fail "Invalid QualityFlag"

parseScore :: Parser TYS.Score
parseScore = double 

parseStrandSign :: Parser TYS.StrandSign
parseStrandSign = 
               (char '+' *> return TYS.Forward)
           <|> (char '-' *> return TYS.Reverse)
           <|> (char '.' *> return TYS.Unknown)
           <|> fail "Invalid QualityFlag"

swapSide :: Parser Char
swapSide =
            (char 'L' *> return 'R')
        <|> (char 'R' *> return 'L')
        <|> (char 'D' *> return 'A')
        <|> (char 'A' *> return 'D')
        <|> fail "Invalid Side in Info field"


