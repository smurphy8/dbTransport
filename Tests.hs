{-# LANGUAGE OverloadedStrings#-}
module Tests where 
import Prelude
import DatabaseTransport 
import System.Environment
import System.Directory
import Control.Monad
import Control.Applicative
import Data.Text
import qualified Text.Parsec as P
import Types
import qualified Data.List as L
import Filesystem.Path
import System.IO


-- | Data Stubs 
testDirectory = "./testArchive/"
testFile = "./testArchive/105/2065/2013-03-13.txt"

testRunConfig = RunConfig testTimeStart testTimeEnd testDirectory

testTimeStart = parseArchiveTime "2013-02-28 00:11:45"

testTimeEnd = parseArchiveTime "2013-02-28 00:12:45"

fileTestTimeStart = parseArchiveTime "2013-01-00 00:00:00"
fileTestTimeEnd = parseArchiveTime "2013-03-28 00:12:45"

testLine :: Text 
testLine = "2013-02-28 00:12:45,0\n"

testDatabaseConfig = MongoConfig "127.0.0.1" "test" "onping_tag_history"

runTests :: IO () 
runTests = do 
--  print "running testRetrieveSubFolders" >> (testRetrieveSubFolders )>>=(\x -> print x)
  print "running testGetLocationPaths" >> (testRetrieveSubFolders )>>=(\x -> print x)
  print "running testGetParamPaths" >> (testGetParamPaths ) >>= (\x -> print x)
  print "running testGetParamFileNames" >> (testGetParamFileNames ) >>= (\x -> print x)
  print "running testParsec"            >> return (testParsecExperiment) >>= (\x -> print x)
  print "running mkFileFilter" >> testMkFileFilter
  print "running testBuildMongoRecords" >> testBuildMongoRecords >>= (\x -> print (L.take 10 x))
  print "running testInsertMongoRecords" >> testInsertMongoRecords >>= (\x -> print x)
  print "testDirectory is : " >> print testDirectory
  print "testFile is: " >> print testFile 
  print "testMongoDBCfg is:" >> print testDatabaseConfig
  print "filterStartTime is:" >> print testTimeStart 
  print "filterEndTime is :" >> print testTimeEnd
  print "fullInsertTest" >> testImportOnpingHistory
  print "test complete" 



testMkFileFilter = do
  file <- L.head.L.head.L.head <$> testGetParamFileNames
  print "file Under test is " >> print file
  print "paramfiletest"
  let filter = mkDateRangeFileFilter (fileTestTimeStart >>= (\st -> return $ StartTime st)) (fileTestTimeEnd >>= (\end -> return $ EndTime end))
      eFiltFcn = getFileFilter <$> filter
      tst = getParamTime $ file
  print "after conversion" >> print tst
  print $ eFiltFcn >>= (\filtFcn -> return (filtFcn file) )
      

      
testImportOnpingHistory = do 
  importOnpingHistory defaultDatabaseConfig testRunConfig

testRetrieveSubFolders :: IO [DatedFile]
testRetrieveSubFolders = do 
  retrieveSubFolders testDirectory


testGetLocationPaths :: IO [LocationPath]
testGetLocationPaths = do 
  getLocationPaths testDirectory

testGetParamPaths :: IO [[ParamPath]]
testGetParamPaths = do 
  lpaths <- (getLocationPaths testDirectory)
  mapM getParamPaths lpaths


testGetParamFileNames :: IO [[[ParamFile]]]  
testGetParamFileNames =  do
  lpaths <- (getLocationPaths testDirectory)
  pPaths <- mapM getParamPaths lpaths
  pFNames <- mapM (mapM getParamFileNames) pPaths
  return pFNames


testBuildMongoRecords = do
  let                            
      s = testTimeStart
      e = testTimeEnd
  lst <- testGetParamFileNames 
  buildMongoRecords (dateRangeFilter s e) $ L.head.L.head.L.head $ lst



testParsecExperiment = do
   buildOnpingTagHistory (NameAndLine "323" testLine)


-- | testFile = "./testArchive/105/2065/2013-03-13.txt"


testInsertMongoRecords = do 
  records <- testBuildMongoRecords 
  insertTagHistoryList testDatabaseConfig records 
      

  
