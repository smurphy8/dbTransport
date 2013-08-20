{-# LANGUAGE OverloadedStrings#-}

module DatabaseTransport where 
import Prelude hiding (FilePath)
import qualified Prelude as Pl
import System.Locale
import Data.Aeson
import Data.Text
import Data.Time
import Data.Maybe
import Database.MongoDB
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
import Control.Concurrent.Spawn (parMapIO_,parMapIO)



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

buildMongoRecords ::  (OnpingTagHistory -> Maybe OnpingTagHistory) -> ParamFile ->
                     IO [OnpingTagHistory]
buildMongoRecords fltr (ParamFile (DatedFile _ pFile))  = do 
  hPidFile <- openPidFileObj pFile
  let ePidNum = (toText windows).dirname $ pFile --error if empty
  case ePidNum of 
    Left _ -> return []
    Right pidNum -> do  
                lst <- runOnpingHistoryParser pidNum hPidFile
                SIO.hClose hPidFile
                return $ catMaybes $ fltr <$>  lst


 




-- | The parser below is dumb and doesn't check for repeats, use your mongo Index unique true to ensure no repeats

runOnpingHistoryParser pidNum hndle = do 
  pidLines <- getPidLines hndle
  let dirtyList = catMaybes.rights $ buildOnpingTagHistory.(\line ->  NameAndLine pidNum line) <$> pidLines
  return dirtyList 






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

othToDocument :: OnpingTagHistory -> Maybe Document
othToDocument (OnpingTagHistory (Just dte) (Just b) (Just c) ) = Just ["time" =: dte, "pid" =: b , "val" =:c]
othToDocument _ = Nothing 


fullDateString = P.many (P.noneOf ",")
valueString = do
  P.many (P.noneOf "\n")


-- | insertTagHistoryListWith Filter uses a filter closure that operates
-- on an OnpingTagHistory

-- | (OnpingTagHistory -> Maybe OnpingTagHistory)
insertTagHistoryList _ [] = return (Right ())
insertTagHistoryList mcfg opthList = do 
  print opthList
  pipe <- runIOE $ connect (host $ mongoHost mcfg)
  e <- access pipe master (mongoDB mcfg) (runDB mcfg opthList)
  close pipe 
  return e 

runDB mcfg  opthList = do 
  insertAll_ (mongoCollection mcfg) (catMaybes (convert <$> opthList))
      where convert :: OnpingTagHistory -> Maybe Document
            convert ix = do                        
                       othToDocument ix

-- |Some simpleFilters predefined

-- |Identity Filter

idFilter :: OnpingTagHistory -> Maybe OnpingTagHistory
idFilter = return 
  
-- |Date Range Filter
dateRangeFilter :: Maybe UTCTime -> Maybe UTCTime -> OnpingTagHistory -> Maybe OnpingTagHistory
dateRangeFilter (Just st) (Just end) o@(OnpingTagHistory (Just t) (Just p) (Just v) )
    | (st < end) && (st <= t) && (end > t) = Just o
    | otherwise = Nothing
dateRangeFilter _ _ _ = Nothing
  

  


defaultDatabaseConfig = MongoConfig "127.0.0.1" "test" "onping_tag_history"


importOnpingHistory mcfg rcfg = do 
  let importFilter = dateRangeFilter (startDate rcfg) (endDate rcfg)
  locationPaths <- getLocationPaths (archivePath rcfg) 
  paramPaths <- mapM getParamPaths locationPaths
  paramFilesNest <- mapM (mapM getParamFileNames) paramPaths
  let paramFilesList :: [ParamFile]
      paramFilesList =  L.concat.L.concat $ paramFilesNest
  opthList <- (buildMongoRecords importFilter) `parMapIO` paramFilesList
  insertTagHistoryList defaultDatabaseConfig `parMapIO_` opthList