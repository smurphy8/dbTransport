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
import System.IO

testDirectory = "./testArchive/"
testFile = "./testArchive/105/2065/2013-03-13.txt"

testMongoDBHost :: Text
testMongoDBHost  = "localhost" 

runTests :: IO () 
runTests = do 
--  print "running testRetrieveSubFolders" >> (testRetrieveSubFolders )>>=(\x -> print x)
  print "running testGetLocationPaths" >> (testRetrieveSubFolders )>>=(\x -> print x)
  print "running testGetParamPaths" >> (testGetParamPaths ) >>= (\x -> print x)
  print "running testGetParamFileNames" >> (testGetParamFileNames ) >>= (\x -> print x)
  print "running testBuildMongoRecords" >> testBuildMongoRecords >>= (\x -> print x)


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
  lst <- testGetParamFileNames 
  buildMongoRecords $ L.head.L.head.L.head $ lst




testLine :: Text 
testLine = "2013-02-28 00:12:45,0\n"

testParsecExperiment = do
   buildOnpingTagHistory (NameAndLine "323" testLine)


-- | testFile = "./testArchive/105/2065/2013-03-13.txt"

