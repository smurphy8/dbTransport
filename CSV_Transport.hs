{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module CSV_Transport where 

import Data.Time
import Data.Csv
import Prelude
import DatabaseTransport
import Control.Applicative
import qualified Data.Map.Strict as M
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

timeStart = parseArchiveTime "2012-08-02 00:00:00"

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

emptyReportRow t = OPRR t (S.empty)

makeOnpingReportMap :: V.Vector OnpingTagHistory -> OnpingReport
makeOnpingReportMap v = OnpingReport $ V.foldl' foldFcn M.empty v
                          where foldFcn = (\opr cand@(OnpingTagHistory (Just t) pid v) ->
                                            case M.lookup t opr of
                                              (Just reportRow) -> M.insert t (insertOnpingReportRow cand reportRow ) opr
                                              Nothing          -> M.insert t (emptyReportRow t) opr ) 



makeReportVector :: OnpingReport -> V.Vector OnpingReportRow
makeReportVector (OnpingReport onpingR)  = V.fromList $  M.foldl foldFcn [] onpingR
  where foldFcn = (\rList rr -> rr:rList)

makeLocationCSV f = do
  putStrLn "Get Time"
  t  <- getCurrentTime
  putStrLn "Get Location"  
  lfp <- getLocationPaths f
  let lp = (LocationPath (DatedFile t f))
  putStrLn "put params"      
  pp <- getParamPaths lp
  pf <- mapM getParamFileNames pp
  putStrLn "build data"      
  othLst <- mapM (buildMongoRecords $ dateRangeFilter timeStart timeEnd) (concat pf)
  let othSet = S.fromList $ concat othLst
      names = V.fromList.L.reverse $ "time" : ( S.toList $ S.map (BC.pack.show.pid)  othSet      )
  print names
  putStrLn "build CSV"       
  B.writeFile "report_output.csv"  $ LB.toStrict $  encodeByName names $ makeReportVector.makeOnpingReportMap.V.fromList.S.toList $ othSet
  putStrLn "done"



