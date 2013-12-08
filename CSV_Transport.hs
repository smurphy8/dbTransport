{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module CSV_Transport where 

import Data.Time
import Data.Csv
import Prelude hiding (FilePath)
import DatabaseTransport
import Control.Applicative
import Control.Concurrent.Spawn
import Filesystem ()
import Filesystem.Path (FilePath)
import Debug.Trace
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
-- import Control.Concurrent.Async
import Types


-- | Makes a CSV of all the parameters of a certain site with pid vals as column headers

-- makeLocationCSV :: FilePath -> IO ()

timeStart :: Maybe UTCTime
timeStart = parseArchiveTime "2012-08-02 00:00:00"

timeEnd :: Maybe UTCTime
timeEnd = parseArchiveTime "2012-08-16 00:12:45"

data OnpingReportRow = OPRR { oprrTime :: UTCTime, oprrData :: S.Set OnpingTagHistory}
  deriving (Eq, Show)


newtype OnpingReport = OnpingReport (M.Map UTCTime OnpingReportRow)
   deriving (Eq,Show)
            
instance ToNamedRecord OnpingReportRow where 
  toNamedRecord (OPRR t setOD) = let
    oD         =  V.fromList.S.toList $ setOD
    timeResult = "time" .= (encodeArchiveTime.Just $  t)
    valNames   =  (BC.pack.show.pid) `V.map` oD
    valResult  =  V.zipWith (\d n -> n .= d) (val `V.map` oD) (valNames)
    in namedRecord $ timeResult :  (V.toList valResult)


insertOnpingReportRow :: OnpingTagHistory -> OnpingReportRow -> OnpingReportRow
insertOnpingReportRow tgt  o@(OPRR tm od) = case S.member tgt  od of
  True -> o
  False -> OPRR tm (S.insert tgt od )

emptyReportRow :: NameSet -> UTCTime -> OnpingReportRow
emptyReportRow ns t = OPRR t (S.map (\p -> p { time = (Just t)} ) ns)

nameSet :: S.Set OnpingTagHistory -> S.Set OnpingTagHistory
nameSet s = S.map nothingButTheName s
    where nothingButTheName (OnpingTagHistory _ p _) = OnpingTagHistory Nothing p Nothing

type NameSet = S.Set OnpingTagHistory

makeOnpingReportMap :: NameSet -> V.Vector OnpingTagHistory -> OnpingReport
makeOnpingReportMap ns v = OnpingReport $ V.foldl' foldFcn M.empty v
    where foldFcn = (\opr cand@(OnpingTagHistory (Just t) _ _) ->
                         case M.lookup t opr of
                           (Just reportRow) -> M.insert t (insertOnpingReportRow cand reportRow ) opr
                           Nothing          -> M.insert t (insertOnpingReportRow cand $ (emptyReportRow ns t)) opr ) 



makeReportVector :: NominalDiffTime -> OnpingReport -> V.Vector OnpingReportRow
makeReportVector delta (OnpingReport onpingR)  = V.fromList $  M.foldl foldFcn [] onpingR
  where foldFcn [] rr_target = rr_target : []
        foldFcn rr_list@((OPRR tlast _):_) rr_target@(OPRR t _) 
            |(abs (diffUTCTime tlast t)) >= (abs delta) = rr_target:rr_list
            | otherwise = rr_list

makeLocationCSV :: FilePath -> IO () 
makeLocationCSV f = do
  putStrLn "Get Time"
  t  <- getCurrentTime
  putStrLn "Get Location"  
--  lfp <- getLocationPaths f
  let lp = (LocationPath (DatedFile t f))
  let selectedFilter =  dateRangeFilter timeStart timeEnd -- idFilter
      delta = realToFrac 60
  putStrLn "put params"      
  pp <- getParamPaths lp
  !pf <- (mapM getParamFileNames pp)>>= (\lst -> return $ concat lst)
  putStrLn "build data"      
  !othLst <- parMapIO (buildMongoRecords $ selectedFilter ) (pf)
  putStrLn "build names"         
  let othSet = S.fromList $ concat othLst
      ns = nameSet othSet
      names = V.fromList $ "time" : ( S.toList $ S.map (BC.pack.show.pid)  othSet      )
  putStrLn "build CSV"
  let repVect = (makeReportVector delta).(makeOnpingReportMap ns).V.fromList.S.toList $ othSet  
  B.writeFile "report_output.csv"  $ LB.toStrict $  encodeByName names   $  repVect 
  putStrLn "done"



