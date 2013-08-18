
{-# LANGUAGE OverloadedStrings#-}
module Main where 
import DatabaseTransport 
import System.Environment
import System.Directory
import Control.Monad
import Tests
import Data.Text
import Data.List
import System.IO





main :: IO () 
main = do 
  runTests
  print "test complete"


-- | To run from command line
-- main :: IO () 
-- main = do 
--   args <- getArgs
--   progName <- getProgName 
--   putStrLn "The Arguments Are" 
--   mapM putStrLn args
--   putStrLn "Prog Name is:"
--   putStrLn progName
