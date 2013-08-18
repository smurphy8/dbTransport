{-# LANGUAGE OverloadedStrings#-}
module Tests where 
import Prelude
import DatabaseTransport 
import System.Environment
import System.Directory
import Control.Monad
import Control.Applicative
import Data.Text
import Data.List
import System.IO



testDirectory = "./testArchive/"

testFile = "./testArchive/105/2065/2013-03-13.txt"


testMongoDBHost :: Text
testMongoDBHost  = "localhost" 



runTests :: IO () 
runTests = do 
  print "running testRetrieveSubFolders" >> (testRetrieveSubFolders )>>=(\x -> print x)
  print "running testGetLocationPaths" >> (testRetrieveSubFolders )>>=(\x -> print x)
  print "running testGetParamPaths" >> (testGetParamPaths ) >>= (\x -> print x)
  print "running testGetParamFileNames" >> (testGetParamFileNames ) >>= (\x -> print x)
  print "running testBuildMongoRecord" >> testBuildMongoRecord >>= (\x -> print x)


testRetrieveSubFolders :: IO [DatedFile]
testRetrieveSubFolders = do 
  retrieveSubFolders testDirectory


testGetLocationPaths :: IO [DatedFile]
testGetLocationPaths = do 
  getLocationPaths testDirectory

testGetParamPaths :: IO [[DatedFile]]
testGetParamPaths = do 
  lpaths <- (getLocationPaths testDirectory)
  mapM getParamPaths lpaths


testGetParamFileNames = do
  lpaths <- (getLocationPaths testDirectory)
  pPaths <- mapM getParamPaths lpaths
  pFNames <- mapM (mapM getParamFileNames) pPaths
  return pFNames


testBuildMongoRecord = do 
  buildMongoRecord testFile