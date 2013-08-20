
{-# LANGUAGE OverloadedStrings#-}
module Main where 
import DatabaseTransport 
import System.Environment
import System.Directory
import Control.Monad
import Data.Text
import Data.List
import Tests
import Types
import Args
import System.IO





main :: IO () 
main = do 
  args <- getArgs
  _ <- case filterArgs args of 
         Test -> runTests
         Help -> runHelp
         Run cfg -> importOnpingHistory defaultDatabaseConfig cfg
         Fail -> runFail args
  return ()





-- |CommandLine Options

