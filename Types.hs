{-# LANGUAGE OverloadedStrings, BangPatterns#-}
module Types where 
import Data.Time
import System.Locale
import Prelude
import Safe
import Data.Text

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

parseArchiveValue :: Text -> Maybe Double
parseArchiveValue = readMay.unpack.strip.snd.(breakOnEnd ",") 

parsePidValue :: Text -> Maybe Int 
parsePidValue = readMay.unpack

buildOnpingTagHistory :: NameAndLine -> Either String OnpingTagHistory
buildOnpingTagHistory (NameAndLine pidT line) = let time = parseArchiveTime line
                                                    val  = parseArchiveValue line
                                                    pid  = parsePidValue line
                                                    oth  = (OnpingTagHistory time pid val)
                                                in case oth of 
                                                    (OnpingTagHistory (Just x) (Just y) (Just z))  ->  Right oth
                                                    (OnpingTagHistory (Just _) (Just _) Nothing)   ->  Left  "missing Val"
                                                    (OnpingTagHistory (Just _) Nothing (Just _))   ->  Left  "missing pid"
                                                    (OnpingTagHistory (Just _) Nothing Nothing)    ->  Left  "missing time and pid"
                                                    (OnpingTagHistory Nothing (Just _) (Just _))   ->  Left  "missing time"
                                                    (OnpingTagHistory Nothing (Just _) Nothing)    ->  Left  "missing time and val"
                                                    (OnpingTagHistory Nothing Nothing (Just _))    ->  Left  "missing time and pid"
                                                    (OnpingTagHistory Nothing Nothing Nothing)     ->  Left  "missing all"

