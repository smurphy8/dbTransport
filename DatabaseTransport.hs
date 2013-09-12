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
  let ePidNum = (toText posix).dirname $ pFile --error if empty
  case ePidNum of 
    Left _ -> putStrLn "error parsing dir" >> return []
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
  print "connecting"
  pipe <- runIOE $ connect (host $ mongoHost mcfg)
  print "accessing"
  e <- access pipe master (mongoDB mcfg) (runDB mcfg opthList)
  print "closing"
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
  

  
mkDateRangeFileFilter :: Maybe (StartTime UTCTime )-> Maybe (EndTime UTCTime)-> Either Text FileFilter
mkDateRangeFileFilter (Just (StartTime st)) (Just (EndTime end))
    |(st < end) = let getParamTime :: ParamFile -> Maybe UTCTime
                      getParamTime = parseFileDate.show.filename.touchFile.getParamFile 
                      checkParamTime :: ParamFile -> UTCTime -> Maybe ParamFile
                      checkParamTime p t
                        |(st <= t) && (end > t) = Just p
                        |otherwise = Nothing
                      filterFcn :: ParamFile -> Maybe ParamFile
                      filterFcn pf = (getParamTime >=> (checkParamTime pf)) pf
                  in Right $ FileFilter filterFcn 


    |otherwise = Left $  ("Error Expecting Start less than End Recieved: Start = " `Data.Text.append` (pack.show $ st))
                         `Data.Text.append`
                         ("End = " `Data.Text.append` (pack.show $ end ))
                 

mkDateRangeFilter _ _ = Left "Error Missing parameter"

                  

defaultDatabaseConfig = MongoConfig "10.61.187.194" "onping_production" "onping_tag_history"


importOnpingHistory mcfg rcfg = do 

  let importFilter = dateRangeFilter (startDate rcfg) (endDate rcfg)
  putStrLn "Getting Location Paths"
  locationPaths <- getLocationPaths (archivePath rcfg) 
  print locationPaths
  putStrLn "getting Param Paths"
  paramPaths <- mapM getParamPaths locationPaths
  print paramPaths
  putStrLn "config options"
  print rcfg
  putStrLn "getting ParamFile Names"
  paramFilesNest <- mapM (mapM getParamFileNames) paramPaths
  let paramFilesList :: [ParamFile]
      paramFilesList =  L.concat.L.concat $ paramFilesNest
  mapM print paramFilesList
  putStrLn "Building mongo records"
--  buildAndInsert rcfg `mapM` paramFilesList
  putStr "done"
  opthList <- (buildMongoRecords importFilter) `mapM` paramFilesList
  putStrLn "Inserting mongo Records"
  insertTagHistoryList defaultDatabaseConfig `mapM_` opthList

buildAndInsert rcfg x = do
  let importFilter = dateRangeFilter (startDate rcfg) (endDate rcfg)
  opth <- (buildMongoRecords importFilter) x
  insertTagHistoryList defaultDatabaseConfig opth
  
