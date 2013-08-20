{-# LANGUAGE OverloadedStrings#-}

module DatabaseTransport where 
import Prelude hiding (FilePath)
import qualified Prelude as Pl
import System.Locale
import Data.Aeson
import Data.Text
import Data.Time
import Data.Maybe

import Types
import qualified Text.Parsec as P
import Text.Parsec.Text
import qualified Data.Text.IO as TIO
import Control.Monad
import Data.Either
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import qualified System.IO as SIO
import qualified Data.List as L
import Data.Function 
import System.Directory
import Filesystem 
import Filesystem.Path
import Filesystem.Path.Rules
import Control.Applicative




-- |Takes the Archive Root and returns a list of the locations
-- | Builders for specific types of files
-- | Top folder level of the archive Tree
-- Get Location Paths are folders named for location Id's

getLocationPaths :: FilePath -> IO [LocationPath]
getLocationPaths arcRoot = do
  sf <- retrieveSubFolders arcRoot
  return $ (\x -> LocationPath x)  <$> sf

-- | Param paths are the next nest down, with 
-- the raw parameter ids making the names
getParamPaths :: LocationPath -> IO [ParamPath] 
getParamPaths (LocationPath (DatedFile _ lPath)) = do
  sf <- retrieveSubFolders lPath
  return $ (\x -> ParamPath x) <$> sf

-- |These are the files inside each raw parameter Id folder
-- | They are named after dates and contain OnpingTagHistory records
getParamFileNames :: ParamPath -> IO [ParamFile]
getParamFileNames (ParamPath (DatedFile _ pPath)) = do
  sf <- retrieveSubFiles pPath
  return $ (\x -> ParamFile x) <$> sf

-- | Dated File Builders for the file system
-- makes a DatedFile, a type with a date modified extracted to the front
makeDatedFile :: FilePath -> IO DatedFile
makeDatedFile f = do 
  date <- getModified f
  return $ DatedFile date f

-- | gets all the Folder Objects in a directory 
-- makes them into DatedFiles      
retrieveSubFolders :: FilePath -> IO [DatedFile]
retrieveSubFolders dir = do 
  dirs <- listDirectory dir
  filteredDirs <- (filterM isDirectory dirs)
  tFiles <- mapM makeDatedFile filteredDirs
  return $ L.sort (tFiles)

retrieveSubFiles :: FilePath -> IO [DatedFile]
retrieveSubFiles dir = do 
  dirs <- (listDirectory dir)
  filteredDirs <- (filterM isFile dirs)
  tFiles <- mapM makeDatedFile filteredDirs
  return $ L.sort (tFiles)
  

{-| Given a path to an archive file
containing entries of the form:
"2013-02-28 00:00:00,0"
an OnpingTagHistory is produced
|-}

buildMongoRecords :: ParamFile -> IO [OnpingTagHistory]
buildMongoRecords (ParamFile (DatedFile _ pFile)) = do 
  hPidFile <- openPidFileObj pFile
  let ePidNum = (toText windows).dirname $ pFile --error if empty
  case ePidNum of 
    Left _ -> return []
    Right pidNum -> do  
                lst <- runOnpingHistoryParser pidNum hPidFile
                SIO.hClose hPidFile
                return $ lst


 



exit = left


runOnpingHistoryParser pidNum hndle = do 
  pidLines <- getPidLines hndle
  print pidNum
  return $ catMaybes.rights $ buildOnpingTagHistory.(\line ->  NameAndLine pidNum line) <$> pidLines





openPidFileObj :: FilePath -> IO Handle
openPidFileObj fPath = openTextFile fPath ReadMode
 

getPidLines hPidFile = do
  pidFile <- TIO.hGetContents hPidFile
  return $ Data.Text.lines pidFile
  


-- | From O'Sullivan, but adapted to use Text

parseEntry :: Maybe Int -> Text -> Either P.ParseError (Maybe OnpingTagHistory)
parseEntry pid i = P.parse (entryString pid) "(unknown)" i


-- | Entry string takes advantage of the monadic form of Maybe to short circuit missing data pieces
entryString mpid = do
  fDate <- fullDateString
  P.char ','
  val <- valueString
  let oth = do
        d <- parseArchiveTime' fDate 
        v <- parseArchiveValue' val
        pid <- mpid
        return $ OnpingTagHistory (Just d) (Just pid) (Just v)        
  return $ oth



buildOnpingTagHistory :: NameAndLine -> Either P.ParseError (Maybe OnpingTagHistory)
buildOnpingTagHistory (NameAndLine pidT line) = parseEntry (parsePidValue pidT) line
                                               
  
fullDateString = P.many (P.noneOf ",")
valueString = do
  P.many (P.noneOf "\n")

