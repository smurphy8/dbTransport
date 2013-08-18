{-# LANGUAGE TupleSections, OverloadedStrings,BangPatterns #-}
module Handler.Home where
import System.Directory
import System.Locale
import System.IO
import System.IO.Error(catch)
import System.Log.FastLogger
import GHC.Conc
import Data.Aeson
import Data.Text (unpack,pack,breakOn,breakOnEnd,concat,append,strip)
import Safe (readMay)
import System.Locale
import System.Timeout
import Data.Maybe
import Yesod
import qualified Data.List as L
import qualified Data.Time as Time
import qualified Database.Persist.MongoDB as PMDB
import qualified Database.MongoDB as MDB
import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.


archiveControlProcess :: Widget
archiveControlProcess = toWidget $ [julius| 
  function archiveControl($scope,$http) { 
    $scope.locations=[];
    $scope.parms = [];
    $scope.files = [];
    $scope.getLocations=function () { 
                      $http.get("@{LocationsR}").
		               success(function(data){
                                                     $scope.locations=$scope.locations.concat(data);

					       });
				  }
    $scope.getParameters=function (location) { 
                                                              
			    $http({
                              method:'GET',
                              url:"@{ParametersR}",
                              params:{"locId":location.locId}
                            }).success(function(data){
                                                     $scope.parms=$scope.parms.concat(data);
                                                     for(var i = 0 ;i<$scope.parms.length;++i)
                                                            {
                                                              
                                                              $scope.getFiles($scope.parms[i]);
                                                            }
                                                   });
                                      }
                                                         
    $scope.getFiles = function (parm){
                           $http({
                             method:'GET',
                             url:"@{HistoryR}",
                             params:{locId:parm.locId,parmId:parm.parmId},
                           }).success(function(data){
                                                    $scope.files=$scope.files.concat(data);
                                                  });
                      }

    $scope.disabled={};
    $scope.status={};
    $scope.lock={};
    $scope.parCount=0;
    $scope.parMax=100;
    $scope.pCounter = -1;
    $scope.currIter=-1;//So I can use a while (not a do while)
    $scope.parseArchiveFile = function (files){
                                //Callbacks for closure to maintain local scope in ajax
                                var callBckClosureError = function (file){
                                                            return function (data) { 
                                                                         file["disabled"] = true;
                                                                         file["status"]="btn-danger"
                                                                         if($scope.pCounter<$scope.parMax)
                                                                           {
                                                                            $scope.pCounter--;
                                                                             $scope.parseArchiveFile(files);
                                                                           }
                                                                         else
                                                                             {

                                                                               $scope.pCounter--;
                                                                             }
                                                                       };
                                                          }
  
  
                                var callBckClosureSuccess = function (file) {
                                                              return function(data){
                                                                           if(data=="\"Done\"")
                                                                                 {
                                                                                   file["disabled"] = true;
                                                                                   file["status"] = "btn-success";
                                                                                 }
                                                                           else
                                                                               {
                                                                                 file["disabled"] = true;
                                                                                 file["status"]= "btn-danger";
                                                                               }
                                                                           if($scope.pCounter<$scope.parMax)
                                                                                 {
                                                                                   
                                                                                    $scope.pCounter--;
                                                                                   $scope.parseArchiveFile(files);
                                                                                   }
                                                                           else
                                                                               {
                                                                                 
                                                                                 $scope.pCounter--;
                                                                               }
                                                                         }
                                                            }




                                
                                
                               
                                        
                                while( $scope.currIter < files.length && $scope.pCounter < $scope.parMax)
                                       {
                                         $scope.pCounter++;
                                         $scope.currIter++;
                                         if(!files[$scope.currIter].disabled){
                                                  $http({
                                                    method:'POST'
                                                  , url:"@{ParseArchiveFileR}"
                                                  , data: [files[$scope.currIter].locId,"/",files[$scope.currIter].parmId,"/",files[$scope.currIter].fileId]
                                                  }).success(callBckClosureSuccess(files[$scope.currIter])).error(callBckClosureError(files[$scope.currIter]));
                                                }
                                       }

                              }
                            
		$scope.parseOneFile=function(file) {
            $http({
                method:'POST'
                ,url:"@{ParseArchiveFileR}"
                ,data:[file.locId,"/",file.parmId,"/",file.fileId]
                }).success(function(data){ 
                                if(data=="\"Done\"")
                                        {
                                         file["disabled"] = true;
                                         file["status"] = "btn-success";
                                        }
                                        else
                                        {
                                        file["disabled"] = true;
                                        file["status"]= "btn-danger";
                                        }
                                   });
                                   }
    }
    
    |]



getHomeR :: Handler RepHtml
getHomeR = do 
   defaultLayout $ do
   addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js" 
   addScriptRemote "https://ajax.googleapis.com/ajax/libs/angularjs/1.0.6/angular.min.js" 
   archiveControlProcess
   [whamlet|
      <div ng-app> 
       <div .row ng-controller="archiveControl">
          <div> Par Count = {{pCounter}} Curr Iter = {{currIter}}
          <button .btn .btn-large ng-click="getLocations()"> Get Locations
          <button .btn .btn-success ng-click="parseArchiveFile(files)"> Mongo that Shiz
          <button .btn ng-click="pCounter=0">Reset Par Count
          <input type="number" ng-model="parMax">
          <div .row .span3>
              <div  ng-repeat="location in locations" .span1 .row>
                  <button .btn .btn-large ng-click="getParameters(location)">{{location.locId}}
          <div .row .span3>
              <div .span2  ng-repeat="parm in parms" .row>
                  <button .btn ng-click="getFiles(parm)"> {{parm.locId}} {{parm.parmId}}
          <div .row .span3>
              <div .span3  ng-repeat="file in files">
                   <button .btn .btn-mini ng-disabled="file.disabled" ng-class="file.status" ng-click="parseOneFile(file)">{{file.locId}} {{file.parmId}}  {{file.fileId}}
                               
                               
         |]




-- getHomeR :: Handler RepHtml
-- getHomeR = do 
--    defaultLayout $ do
--    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js" 
--    addScriptRemote "https://ajax.googleapis.com/ajax/libs/angularjs/1.0.6/angular.min.js" 
--    archiveControlProcess
--    [whamlet|
--       <div ng-app> 
--        <div .row ng-controller="archiveControl">
--           <button .btn .btn-large ng-click="getLocations()"> To the Cloud!
--           <div .row .span10>
--               <div .row  ng-repeat="location in locations" .span3>
--                   <button .btn .btn-large ng-click="getParameters(location)">{{location.txt}}
--                 <div .offset1 ng-repeat="parm in locations[$index].parms" .row>
--                                <button .btn ng-click="getHistory(location,parm)"> 
--                                    {{parm.txt}}
--                                <div .offset1 ng-repeat="file in parm.files">
--                                   <button .btn .btn-mini ng-disabled="file.disabled" ng-class="file.status" ng-click="parseArchiveFile(location.txt,parm.txt,file)"> {{file.txt}}
                               
                               
--          |]



windowsMode = "C:/Users/Administrator/Dropbox/www2/1.4.0.5/1.4.0.5/OnPing Automation/bin/Debug/Archive/"

linuxMode = "/home/scott/tmp/"

baseMode = windowsMode

-- | /archive/locations LocationsR GET
getLocationsR :: Handler RepJson
getLocationsR = do
  adminDirs <- liftIO $ getDirectoryContents baseMode  >>= return .filter (`notElem` [".", ".."])
  jsonToRepJson.toJSON $  (\x-> Import.object [("locId" .= x)]) <$>  adminDirs 


-- |/archive/parameters ParametersR GET
getParametersR :: Handler RepJson
getParametersR = do 
  mid <- lookupGetParam "locId"
  case mid of 
    (Just lid) -> do
                   parmDirs <- liftIO $ getDirectoryContents (baseMode ++ (unpack lid)) >>= return . filter (`notElem` [".", ".."])
                   jsonToRepJson.toJSON $ (\x -> Import.object [("locId" .= lid), ("parmId".=x)]) <$> parmDirs
    Nothing -> do
      jsonToRepJson.toJSON $ err
          where err::Text
                err = "location not found"



-- | /archive/history HistoryR GET
getHistoryR :: Handler RepJson
getHistoryR =  do 
  mlocId <- lookupGetParam "locId"
  mparmId <- lookupGetParam "parmId"
  case (++)<$>((++).unpack <$> mlocId <*> (Just "/")) <*>  (unpack <$> mparmId) of 
    (Just x) -> do
                 histDir <- liftIO $ getDirectoryContents (baseMode ++ x) >>= return . filter (`notElem` [".", ".."])
                 jsonToRepJson.toJSON $ (\x ->Import.object [("locId" .= mlocId) 
                                                            ,("parmId".= mparmId)
                                                            ,("fileId".= x)]) <$>  histDir 
    Nothing -> do 
      jsonToRepJson.toJSON $ err
        where err::Text
              err = "location not found"





-- | /parse/archivefile ParseArchiveFileR GET
postParseArchiveFileR :: Handler RepJson
postParseArchiveFileR = do   
  master <- getYesod  
  rsltFileName <- parseJsonBody :: Handler (Result [Text])
  
  case rsltFileName of 
    (Error _) -> jsonToRepJson.toJSON $ err
      where 
        err::Text
        err = "invalid File Input"
    (Success fName) ->  do
                          _ <- liftIO $ loggerPutStr (appLogger master)  ((toLogStr.unpack) <$>  fName)
                          inH <- liftIO $ openFile (unpack.(baseMode `append`).Data.Text.concat $ fName) ReadMode
                          _ <- liftIO $ hSetBuffering inH (BlockBuffering (Just 7000)) --240 blockSize
                          rst  <- mongoWrite (readMay.unpack.L.head.(drop 2) $ fName) inH 0 []
                          _ <- liftIO $ hClose inH
                          jsonToRepJson.toJSON $ rst





-- | Simple Parsers for Time and Value 

parseArchiveTime::Time.ParseTime t => Text -> Maybe t
parseArchiveTime = (Time.parseTime defaultTimeLocale "%F %X").unpack.fst.(breakOn ",")

parseArchiveValue :: Text -> Maybe Int
parseArchiveValue = readMay.unpack.strip.snd.(breakOnEnd ",") 


-- | Looping mongoDB writer

--mongoWrite :: Maybe Int -> Handle -> Handler Bool
mongoWrite :: Maybe Int -> Handle -> Int -> [MDB.Document] -> Handler (Maybe Text)
mongoWrite pid inH (!acm) lst = do 
  let parCount = 24000
  master <- getYesod
  
  eof <- liftIO $ hIsEOF inH
  case eof of 
    True -> do  
             
             _ <- runDB $! do 
                    MDB.insertAll "onping_tag_history" (L.nub lst)
             return (Just "Done")
    False -> do
             ln <- liftIO ( hGetLine inH >>= (\x -> return $ pack x) )
             let value = (parseArchiveValue ln) 
                 time  = (parseArchiveTime ln) 
             case seq <$> time <*> value of
               
               Nothing -> return Nothing
               _ ->  case (acm < parCount) of
                       True -> do
                         mongoWrite pid inH (acm + 1) ( (PMDB.toInsertFields $! OnpingTagHistory pid time value) : lst)
                       False -> do 
                               _ <- runDB $ do 
                                           --  _ <- liftIO $ loggerPutStr (appLogger master)  (toLogStr.show <$> lst)
                                             MDB.insertAll_ "onping_tag_history" (L.nub lst)
                               
                               mongoWrite pid inH 0 [] -- Reset accumulator
                               

timeParseCheck :: Maybe Int -> Handle -> Handler (Maybe Time.UTCTime)
timeParseCheck pid inH = do 
  ln <- liftIO $ hGetLine inH >>= (\x -> return $ pack x) 
  return (parseArchiveTime ln)

valueParseCheck :: Maybe Int -> Handle -> Int -> [MDB.Document] -> Handler (Maybe Text)
valueParseCheck _ inH _ _ = do 
  ln <- liftIO $ hGetLine inH >>= (\x -> return $ pack x)  
  return (Just ln)
  
-- ==================================================
-- ==================================================
-- ==================================================
-- ==================================================


-- | TESTS FOR THE TWO Parse needs



 
-- | /parse/time ParseTimeR GET

getParseTimeR :: Handler RepHtml
getParseTimeR = do 
  let tst :: Maybe Time.UTCTime
      tst = parseArchiveTime testStr
      testStr = "2012-10-17 00:00:10,600" 
  defaultLayout $ do 
    [whamlet|
     $maybe t <- tst
       <div> #{show (Time.utcToLocalTime Time.utc t)}
         |]


-- | /parse/value ParseValueR GET

getParseValueR :: Handler RepHtml
getParseValueR = do 
  let val = snd.(breakOnEnd ",") $ testStr
      testStr = "2012-10-17 00:00:10,600" 
  defaultLayout $ do 
    [whamlet|
     <div> Test #{val}
     |]
  