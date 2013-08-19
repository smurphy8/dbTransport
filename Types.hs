{-# LANGUAGE OverloadedStrings, BangPatterns#-}
module Types where 
import Data.Time
import System.Locale
import Prelude hiding (FilePath)
import Safe
import Data.Text
import System.Directory
import Filesystem 
import Filesystem.Path

data OnpingTagHistory = OnpingTagHistory { 
      time :: Maybe UTCTime,
      pid:: Maybe Int,
      val :: Maybe Double
} deriving (Read, Show, Eq,Ord)
data NameAndLine = NameAndLine { nlName::Text, nlLine::Text}



data BuildableObject = B_OTH !OnpingTagHistory
                     deriving (Read,Show,Eq,Ord)



type Buildable a = NameAndLine -> Either String (a,Text)

-- | Simple Parsers for Time and Value 

parseArchiveTime::ParseTime t => Text -> Maybe t
parseArchiveTime = (parseTime defaultTimeLocale "%F %X").unpack.fst.(breakOn ",")

parseArchiveTime'::ParseTime t => String -> Maybe t
parseArchiveTime' = (parseTime defaultTimeLocale "%F %X")


parseArchiveValue :: Text -> Maybe Double
parseArchiveValue = readMay.unpack.strip.snd.(breakOnEnd ",") 


parseArchiveValue' :: String -> Maybe Double 
parseArchiveValue' = readMay


parsePidValue :: Text -> Maybe Int 
parsePidValue = readMay.unpack



-- | Helper Type to pull the date out to the front for sorting against
-- the File Touch Time



data DatedFile = DatedFile { touchDate :: UTCTime,
                           touchFile :: FilePath
                         }

              deriving (Eq,Show,Ord)


-- | Newtypes for Location and PID folders 
newtype LocationPath = LocationPath {getLocationPath :: DatedFile}
    deriving (Eq,Show,Ord)

newtype ParamPath = ParamPath {getParamPath :: DatedFile}
    deriving (Eq,Show,Ord)


newtype ParamFile = ParamFile {getParamFile :: DatedFile}
    deriving (Eq,Show,Ord)
