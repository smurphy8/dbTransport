{-# LANGUAGE OverloadedStrings, BangPatterns#-}
module Types where 
import Data.Time
import System.Locale
import Prelude
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

parseArchiveValue :: Text -> Maybe Int
parseArchiveValue = readMay.unpack.strip.snd.(breakOnEnd ",") 

parsePidValue :: Text -> Maybe Int 
parsePidValue = readMay.unpack

buildOnpingTagHistory :: NumAndLine -> Either String a
buildOnpingTagHistory (NumAndLine pidT line) = let time = parseArchiveTime line
                                                  val  = parseArchiveValue line
                                                  pid  = parsePidValue
                                              in case (OnpingTagHistory time pid val) of 
                                                  good@ (OnpingTagHistory (Just a) (Just b) (Just v)  = Right good
                                                         (OnpingTagHistory (Just _) (Just _) Nothing) = Either "missing Val"
                                                         (OnpingTagHistory (Just _) Nothing (Just _)) = Either "missing pid"
                                                         (OnpingTagHistory (Just _) Nothing Nothing)  = Either "missing time and pid"
                                                         (OnpingTagHistory Nothing (Just _) (Just _)) = Either "missing tim"
                                                         (OnpingTagHistory Nothing (Just _) Nothing)  = Either "missing tim and val"
                                                         (OnpingTagHistory Nothing Nothing (Just _))  = Either "missing time and pid"
                                                         (OnpingTagHistory Nothing Nothing Nothing)   = Either "missing all"

