{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import Options.Applicative (execParser)
import System.IO (hPutStrLn, stdout, stderr)



import Pragmatic.Types
--import Pragmatic.Parse.JSON
import Pragmatic.Junctions
import Pragmatic.JunctionScore
import CommandLineArgs
import File
import Pragmatic.Parse.Junction


chooseParsingFunction :: CommandLineOpts -> (Parser Junction)
chooseParsingFunction args = case fileFormat args of
--    JSON  -> parseAsJSONJunction
    Haarz -> parseAsHaarzJunction  -- This must changed to parseAsGFFJunction, also what if I have more than two options

-- Should write it in the future
--chooseParsingFunction CommandLineOpts{fileFormat=JSON}  = parseAsJSONJunction
--chooseParsingFunction CommandLineOpts{(fileFormat args)==Haarz} = parseAsHaarzJunction

getJunction :: CommandLineOpts -> IO Junctions
getJunction args = do
     junctionFile <- readJunction (filePath args)
     let parserChoice = chooseParsingFunction args
     return $ parseJunctions parserChoice junctionFile


--main :: IO ()
main = do 
     args <- execParser opts
     junctionData <- getJunction args
     let junctionScores = toJunctionScores junctionData
       --junctionScore
     mapM (BL.hPutStrLn stdout) junctionScores -- | junctionScore<-junctionScores]
--     BL.hPut stdout . concat (junctionScores)
     hPutStrLn stderr "Conversion to JunctionScore completed."
     return ()


--(command:argList) <- getArgs
--dispatch command argList

--import Data.Aeson
--import Data.Monoid

--import System.Environment

-- This is bullshit I have to have a simpler readable arg parser 
--dispatch :: String -> [String] -> IO ()
--dispatch "gtf2json" = gtf2json
--dispatch "gtfRecord" = gtfRecord2json
--dispatch "spliceSite" = spliceSite 
--dispatch command = doesntExist command

-- | Retrieving splice sites from which source! It can be from 3 different sources:
-- |   I.   GTF Annotation
-- |   II.  bed12
-- |   III. Test read Alignment bed format
-- | FORGET ABOUT IT NOW!

--doesntExist :: String -> [String] -> IO ()
--doesntExist command _ =
--     putStrLn $ "Clueless of " ++ command

-- | Score every spliceSites 
-- perl all_splits_exact_reads.with_strandspecific_option.pl -o all_ss.dat -P /main_path/ -minread 3 -genome hg19 -strand_specific
-- This is more work than I have thought! Should it be an iteration over all files
-- or should it be just reading one given file?! I have no f*#&! idea!
-- right now file by file then I will changing it if I find out which one is 
-- the best one! 
--spliceSites :: [String] -> IO ()
--spliceSites [fileName] = do
--      putStrLn $ "reading Haarz formated file from " ++ fileName
--      junctionSitesFile <- BL.readFile fileName
--      let junctionSiteLines = C8.lines junctionSitesFile
--      let junctionResults = toJunction junctionSiteLines

--gtfRecord2json :: [String] -> IO ()
--gtfRecord2json [fileName] = do
--     toParse <- BL.readFile fileName
--     case (eitherDecode' toParse :: Either String GTFrecord) of
--       Right val -> print val
--       Left error -> C8.putStrLn $ C8.pack error <> " in " <> toParse


--gtf2json :: [String] -> IO ()
--gtf2json [fileName] = do
--     contents <- BL.readFile fileName
--     putStrLn $ "reading gtf formated file from " ++ fileName
--     let entries = C8.lines contents -- why lines doesn't work on lazy library
--         mainFieldss = [C8.split '\t' entry | entry <- entries]
--         infoFieldss = [C8.split ' ' $ mainFields !! 8 | mainFields <- mainFieldss]
--    print $ head infoFieldss




