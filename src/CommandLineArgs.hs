module CommandLineArgs where --(CommandLineOpts, filePath, opts, fileFormat,FileFormatType) where

import Data.Version (showVersion)
import Options.Applicative
import Data.Text.Format (format)

--import Paths_scoreJunctions (version)

data FileFormatType =  JSON | Haarz deriving (Eq)

instance Show FileFormatType where
      show JSON   = "json"
      show Haarz = "haarz"

data CommandLineOpts = CommandLineOpts
      { 
        filePath :: FilePath
      , fileFormat :: FileFormatType
      , verbose :: Bool 
      } deriving Show

 -- | A helper function that gets the project version from the cabal file.
--versionString :: String
--versionString = showVersion version

-- |Given an input string that should contain an a valid format specifier, return either an error message or FileFormatType
fileFormatReader :: String -> Either String FileFormatType
fileFormatReader "haarz"   = Right Haarz
fileFormatReader "json" = Right JSON
fileFormatReader input      = Left ( "Invalid format: " ++ input )

parseOpts :: Parser CommandLineOpts
parseOpts = CommandLineOpts
     <$> argument str
        (  metavar "PATH_TO_FILE"
        <> help "Path to your file." )
     <*> option (eitherReader fileFormatReader)
        (  long "format"
        <> short 'f'
        <> value Haarz
        <> showDefault
        <> metavar "FORMAT"
        <> help "Format of file; must be a value of \"haarz\" or \"json\"." )
     <*> switch ( long "verbose" <> help "Show output explaining what the command is doing as it runs." <> hidden )


-- | A command line arguments parser for the jucntion file executable.
opts :: ParserInfo CommandLineOpts
opts = info (helper <*> parseOpts)
       (  fullDesc
       <> progDesc "Score the conservation of Splice Junction"
       <> header description )
     where description = "Score the conservation of Splice Junction. Version {0.0.1}" --format "Score the conservation of Splice Junction. Version {0.0.1}" -- [versionString]

--parseAsCommon :: CommandLineOpts -> Bool
--parseAsCommon args = (logFormat args) == NCSACommon
