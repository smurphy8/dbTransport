{-# LANGUAGE OverloadedStrings#-}

module DatabaseTransport where 
import Prelude hiding (FilePath)
import System.Locale
import Data.Aeson
import Data.Text
import Data.Time
import Types
import qualified Data.Text.IO as TIO
import Control.Monad
import qualified System.IO as SIO
import qualified Data.List as L
import Data.Function 
import System.Directory
import Filesystem 
import Filesystem.Path
import Control.Applicative




-- |Takes the Archive Root and returns a list of the locations

getLocationPaths :: FilePath -> IO [DatedFile]
getLocationPaths arcRoot = retrieveSubFolders arcRoot


getParamPaths :: DatedFile -> IO [DatedFile] 
getParamPaths (DatedFile _ lPath) = retrieveSubFolders lPath


getParamFileNames :: DatedFile -> IO [DatedFile]
getParamFileNames (DatedFile _ pPath) = retrieveSubFiles pPath







-- | Helper Type to pull the date out to the front for sorting against
-- the File Touch Time

data DatedFile = DatedFile { touchDate :: UTCTime,
                           touchFile :: FilePath
                         }

              deriving (Eq,Show,Ord)

makeDatedFile :: FilePath -> IO DatedFile
makeDatedFile f = do 
  date <- getModified f
  return $ DatedFile date f
      
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
  



buildMongoRecord fPath = do 
  hPidFile <- openPidFileObj fPath
  pLine <- getPidLine hPidFile
  let pidNum = pack.show.dirname $ fPath 
      nl = NameAndLine pidNum pLine
  print pLine >> SIO.hClose hPidFile >> return () 
--  makeBuildable nl onpingTagBuilder
  
openPidFileObj :: FilePath -> IO Handle
openPidFileObj fPath = openTextFile fPath ReadMode
 

getPidLine hPidFile = TIO.hGetLine hPidFile




  
    
