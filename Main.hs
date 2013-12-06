
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
import CSV_Transport

import System.IO





main :: IO () 
main = do
  makeLocationCSV "/home/scott/programs/oneoff_reports/219"  
  return ()

  -- args <- getArgs
  -- _ <- case filterArgs args of 
  --        Test -> runTests
  --        Help -> runHelp
  --        Run cfg -> putStrLn "begginning" >> importOnpingHistory defaultDatabaseConfig cfg
  --        Fail -> runFail args




-- |Commandline Options

