{-# LANGUAGE OverloadedStrings, BangPatterns #-}



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
import qualified Data.Text.IO as TIO
import Control.Monad
import Data.Either
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import qualified System.IO as SIO
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Attoparsec (many')
import Data.Attoparsec.Text
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
  return $ LocationPath   <$> sf

-- | Param paths are the next nest down, with 
-- the raw parameter ids making the names
getParamPaths :: LocationPath -> IO [ParamPath] 
getParamPaths (LocationPath (DatedFile _ lPath)) = do
  sf <- retrieveSubFolders lPath
  return $ ParamPath  <$> sf

-- |These are the files inside each raw parameter Id folder
-- | They are named after dates and contain OnpingTagHistory records  
getFilterParamFileNames :: FileFilter -> ParamPath -> IO [ParamFile]
getFilterParamFileNames (FileFilter fltrFcn) (ParamPath (DatedFile _ pPath)) = do
  sf <- retrieveSubFiles pPath
  return $ catMaybes $ fltrFcn.ParamFile  <$> sf


getParamFileNames :: ParamPath -> IO [ParamFile]
getParamFileNames (ParamPath (DatedFile _ pPath)) = do
  sf <- retrieveSubFiles pPath
  return $  ParamFile  <$> sf


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
  SIO.hSetBuffering hPidFile (SIO.BlockBuffering (Just 262144))
  let ePidNum = (toText posix).dirname $ pFile --error if empty
  case ePidNum of 
    Left _ -> putStrLn "error parsing dir" >> return []
    Right pidNum -> do                  
                lst <- runOnpingHistoryParser pidNum hPidFile
                SIO.hClose hPidFile
                return $ catMaybes $ fltr <$>  lst

-- | The parser below is dumb and doesn't check for repeats, use your mongo Index unique true to ensure no repeats
runOnpingHistoryParser :: Text -> Handle -> IO [OnpingTagHistory]
runOnpingHistoryParser pidNum hndle = do 
  pidLines <- getPidLines hndle
  return $ catMaybes.rights $ buildOnpingTagHistory.(\(line) ->  NameAndLine pidNum line) <$> pidLines
 



openPidFileObj :: FilePath -> IO Handle
openPidFileObj fPath = openTextFile fPath ReadMode
 

getPidLines hPidFile = do
  pidFile <- TIO.hGetContents hPidFile
  return $ Data.Text.lines pidFile
  


-- | From O'Sullivan, but adapted to use Text

-- parseEntry :: Maybe Int -> Text -> Either P.ParseError (Maybe OnpingTagHistory)
parseEntry pid i = parseOnly (entryString pid)  i


-- | Entry string takes advantage of the monadic form of Maybe to short circuit missing data pieces
entryString mpid = do
  fDate <- newParseTime --fullDateString
  char ','
  val <- double
  -- let oth = do
  --       d <- parseArchiveTime' fDate 
  --       v <- parseArchiveValue' val
  --       pid <- mpid
  return $ (\d -> OnpingTagHistory (Just d) mpid (Just val) ) <$> fDate


newParseTime  :: Parser (Maybe UTCTime)
newParseTime = do
  y <- decimal
  char '-'
  m <- decimal
  char '-'
  d <- decimal
  char ' '
  h <- decimal
  char ':'
  min <- decimal
  char ':'
  sec <- decimal
  let seconds = 3600*h + 60*min + sec
      dTime = secondsToDiffTime seconds      
  return $ (\d -> UTCTime d dTime) <$> (fromGregorianValid y m d )

-- buildOnpingTagHistory :: NameAndLine -> Either P.ParseError (Maybe OnpingTagHistory)
buildOnpingTagHistory (NameAndLine pidT line) = parseEntry (parsePidValue pidT) line

othToDocument :: OnpingTagHistory -> Maybe Document
othToDocument (OnpingTagHistory (Just dte) (Just b) (Just c) ) = Just ["time" =: dte, "pid" =: b , "val" =:c]
othToDocument _ = Nothing 


-- fullDateString :: P.Stream s m Char => P.ParsecT s u m [Char]
fullDateString = many' (notChar ',')

valueString = do
  many' (notChar '\n')


-- | insertTagHistoryListWith Filter uses a filter closure that operates
-- on an OnpingTagHistory

-- | (OnpingTagHistory -> Maybe OnpingTagHistory)
insertTagHistoryList _ [] = return (Right ())
insertTagHistoryList mcfg opthList = do 
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


encodeText f = case toText posix f of 
                 Left t -> t
                 Right t -> t
              

getParamTime :: ParamFile -> Maybe UTCTime
getParamTime = parseFileDate.unpack.(Filesystem.Path.Rules.encode darwin).filename.touchFile.getParamFile 

checkParamTime :: StartTime UTCTime -> EndTime UTCTime -> ParamFile -> UTCTime -> Maybe ParamFile
checkParamTime (StartTime s) (EndTime e) p t
  |(s <= t) && (e > t) = Just p
  |otherwise = Nothing
  
mkDateRangeFileFilter :: Maybe (StartTime UTCTime )-> Maybe (EndTime UTCTime)-> Either Text FileFilter
mkDateRangeFileFilter (Just st@(StartTime s)) (Just en@ (EndTime e))
    |(s < e)    = let filterFcn :: ParamFile -> Maybe ParamFile
                      filterFcn pf = (getParamTime >=> (checkParamTime st en pf)) pf
                  in Right $ FileFilter filterFcn 


    |otherwise = Left $  ("Error Expecting Start less than End Recieved: Start = " `Data.Text.append` (pack.show $ s))
                         `Data.Text.append`
                         ("End = " `Data.Text.append` (pack.show $ e))
                 

mkDateRangeFilter _ _ = Left "Error Missing parameter"

                  

defaultDatabaseConfig = MongoConfig "10.61.187.194" "onping_production" "onping_tag_history"


importOnpingHistory mcfg rcfg = do 
  let strt = startDate rcfg
      end = endDate rcfg
      
      importFilter = dateRangeFilter (startDate rcfg) (endDate rcfg)
      st = strt >>= (\s -> return $ StartTime s)
      en  = end >>= (\e -> return $ EndTime e)
  (Right filterFcn) <- return $  (mkDateRangeFileFilter st en)
      
  putStrLn "Getting Location Paths"
  locationPaths <- getLocationPaths (archivePath rcfg) 
  print locationPaths
  putStrLn "getting Param Paths"
  paramPaths <- mapM getParamPaths locationPaths
  putStrLn "config options"
  print rcfg
  putStrLn "getting ParamFile Names"
  paramFilesNest <- mapM (mapM (getFilterParamFileNames filterFcn)) paramPaths
  let paramFilesList :: [ParamFile]
      paramFilesList = L.concat.L.concat $ paramFilesNest
  putStrLn "Building mongo records"
--  buildAndInsert rcfg `mapM` paramFilesList
  opthList <- (buildMongoRecords importFilter) `mapM` paramFilesList
  putStrLn "Inserting mongo Records"
  insertTagHistoryList defaultDatabaseConfig `mapM_` opthList

buildAndInsert rcfg x = do
  let importFilter = dateRangeFilter (startDate rcfg) (endDate rcfg)
  opth <- (buildMongoRecords importFilter) x
  insertTagHistoryList defaultDatabaseConfig opth
  
