{-# LANGUAGE OverloadedStrings, BangPatterns#-}
module Types where 
import Data.Time
import System.Locale
import Prelude hiding (FilePath)
import Safe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Csv
import Data.Text
import System.Directory
import Filesystem 
import Filesystem.Path


data OnpingTagHistory = OnpingTagHistory { 
      time :: Maybe UTCTime,
      pid:: Maybe Int,
      val :: Maybe Double
} deriving (Read, Show, Eq,Ord)



instance ToNamedRecord OnpingTagHistory where 
  toNamedRecord (OnpingTagHistory time pid val) = namedRecord [ "time" .= (encodeArchiveTime time), (BC.pack.show $ pid ) .= val]

instance ToRecord OnpingTagHistory where 
    toRecord (OnpingTagHistory t p v) = record [toField (encodeArchiveTime t), toField v]

data NameAndLine = NameAndLine { nlName::Text, nlLine::Text}

data BuildableObject = B_OTH !OnpingTagHistory
                     deriving (Read,Show,Eq,Ord)

type Buildable a = NameAndLine -> Either String (a,Text)


newtype FileFilter = FileFilter { getFileFilter :: (ParamFile -> Maybe ParamFile)}

newtype StartTime a = StartTime { getStartTime :: a}
                    deriving (Eq,Read,Show)
                             
newtype EndTime a = EndTime { getEndTime :: a }
                    deriving (Eq,Read,Show)

                             


-- | Simple Parsers for Time and Value 

parseFileDate :: String -> Maybe UTCTime
parseFileDate = (parseTime defaultTimeLocale "%F.txt")

parseArchiveTime::Text -> Maybe UTCTime
parseArchiveTime = (parseTime defaultTimeLocale "%F %X").unpack.fst.(breakOn ",")

parseArchiveTime':: String -> Maybe UTCTime
parseArchiveTime' = (parseTime defaultTimeLocale "%F %X")

encodeArchiveTime :: Maybe UTCTime => String
encodeArchiveTime (Just t) = (formatTime defaultTimeLocale "%F %X") t
encodeArchiveTime Nothing = ""

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



-- | Mongo Config options

data MongoConfig = MongoConfig { 
      mongoHost :: String
      ,mongoDB :: Text 
      ,mongoCollection :: Text
    } deriving (Eq,Read,Show)



data ConfigOptions =  Test | Help | Run RunConfig |Fail


data OS = Windows | Linux

data RunConfig = RunConfig { 
      startDate :: Maybe UTCTime 
      ,endDate :: Maybe UTCTime
      ,archivePath :: FilePath}
               deriving (Show)
