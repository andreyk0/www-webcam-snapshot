{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Data.IORef
import Data.List
import System.Console.CmdArgs
import Webcam
import Yesod


data Opts =
  Opts { listen :: Int
       , videoDevice :: [String]
       , interval :: Int -- ^ refresh interval, seconds
       } deriving(Show, Data, Typeable)

cmdLineOpts :: Opts
cmdLineOpts =
  Opts { listen = 9091 &= help "local port to bind to, defaults to 9091"
       , videoDevice = [] &= help "video device paths to grab frames from, defaults to /dev/video*"
       , interval = 3 &= help "refresh interval (seconds), defaults to 3"
       } &= program "www-webcam-snapshot" &=
                summary "Periodically grabs frames from attached USB webcams and serves them as JPG." &=
                help "Usage: www-webcam-snapshot [--listen 1234] [--video-device /dev/video0] [--video-device /dev/video1]"


data App = App { webcamImages :: [WebcamImage]
               , refreshInterval :: Int -- ^ seconds
               }


mkYesod "App" [parseRoutes|
/          HomeR   GET
/#WebcamId WebCamR GET
|]


instance Yesod App where
  makeSessionBackend _ = return Nothing


getHomeR :: Handler Value
getHomeR = do
  App {..} <- getYesod
  let devices = (flip map) webcamImages $ \WebcamImage{..} ->
                object [ "id"   .= fromEnum webcamId
                       , "path" .= webcamDevice
                       ]
  returnJson $ object [ "refreshInterval" .= refreshInterval
                      , "devices"         .= devices
                      ]


getWebCamR :: WebcamId -> Handler Value
getWebCamR wid = do
  App {..} <- getYesod
  case find ((wid ==) . webcamId) webcamImages
    of Nothing   ->
         notFound
       Just WebcamImage{..} -> do
         ibs <- liftIO $ readIORef imageBytes
         addHeader "Cache-Control" "public, max-age=3"
         sendResponse (typeJpeg, toContent ibs)


main :: IO ()
main = do
  Opts{..} <- cmdArgs cmdLineOpts

  if (interval <= 0)
    then error $ "Refresh interval must be > 0!"
    else return ()

  if (listen <= 0)
    then error $ "Port number must be > 0!"
    else return ()

  wis <- startFrameGrabber interval videoDevice
  warp listen $ App wis interval

