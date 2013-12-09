{-# LANGUAGE OverloadedStrings#-}
module Main where 
-- import DatabaseTransport 

-- import Tests
-- import Types
import CSV_Transport



main :: IO () 
main = do
  makeLocationCSV "/home/scott/programs/oneoff_reports/219"  -- "testArchive/105/" 
  return ()

  -- args <- getArgs
  -- _ <- case filterArgs args of 
  --        Test -> runTests
  --        Help -> runHelp
  --        Run cfg -> putStrLn "begginning" >> importOnpingHistory defaultDatabaseConfig cfg
  --        Fail -> runFail args




-- |Commandline Options

