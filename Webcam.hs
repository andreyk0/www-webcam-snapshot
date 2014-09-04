{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Webcam (
    WebcamId(..)
  , WebcamImage(..)
  , startFrameGrabber
) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import CWebcam
import Data.Data
import Data.IORef
import Data.Monoid
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Yesod (PathPiece)

import qualified Codec.Picture.Jpg as JPG
import qualified Data.ByteString.Lazy as LB


newtype WebcamId = WebcamId Int
  deriving (Data, Enum, Eq, Ord, PathPiece, Read, Show, Typeable)


-- | Webcam Id and Image bytes in the png format
data WebcamImage = WebcamImage { webcamId:: WebcamId
                               , webcamDevice :: FilePath
                               , imageBytes:: IORef LB.ByteString
                               }

getAllVideoDevices :: IO [(WebcamId, FilePath)]
getAllVideoDevices = do
  let videoDevices = (flip map) [0 .. 16] $ \i -> (WebcamId i, "/dev/video" ++ (show i))
  existingDevices <- filterM (doesFileExist . snd) videoDevices
  filterM (isVideoStreamingDevice . snd) existingDevices

isVideoStreamingDevice :: FilePath -> IO Bool
isVideoStreamingDevice fp = do
  vCap <- withV4L2Device fp hasVideoStreamingCapability
  case vCap
    of Right True ->
         return True

       Right False -> do
         warn $ show fp <> " doesn't have video streaming capability, skipping ..."
         return False

       Left e -> do
         err $ show e <> "trying to access " <> fp <> " ... skipping ..."
         return False


getUserVideoDevices :: [FilePath] -> IO [(WebcamId, FilePath)]
getUserVideoDevices userDevFps = do
  let videoDevices = (flip map) (zip [0 ..] userDevFps) $ \(i, vfp) -> (WebcamId i, vfp)

  forM_ videoDevices $ \(_, vd) -> do
    exists <- doesFileExist vd
    if exists
      then return ()
      else error $ show vd <> " does not exist!"

  forM_ videoDevices $ \(_, vd) -> do
    isV <- isVideoStreamingDevice vd
    if isV
      then return ()
      else error $ show vd <> " is not a video streaming device!"

  return videoDevices


startFrameGrabber :: Int -- ^ refresh interval seconds
                  -> [FilePath] -- ^ user-requested video device paths (will grab all available if empty)
                  -> IO [WebcamImage]
startFrameGrabber refreshIntervalSecs userVdevs = do
  vdevs <- if null userVdevs
             then getAllVideoDevices
             else getUserVideoDevices userVdevs

  forM vdevs $ \(wid, vfp) -> do
    imgBytesRef <- newIORef LB.empty
    startSingleWebcam refreshIntervalSecs vfp imgBytesRef
    return $ WebcamImage wid vfp imgBytesRef


startSingleWebcam :: Int -- ^ refresh interval seconds
                  -> FilePath -- ^ device file path
                  -> IORef LB.ByteString -- ^ put generated image bytes here
                  -> IO ()
startSingleWebcam refreshIntervalSecs vfp imgBytesRef = do
  _ <-forkIO $ forM_ [0::Int ..] $ \i -> do
    e <- withV4L2Device vfp $ \vd -> do
      info $ "Starting frame grabber background task " <> show i <> " for " <> show vd
      withFrameGrabber vd (loop)
    err $ show e
    threadDelay delayMicros -- in case it's in the error loop
  return ()

  where loop :: V4L2FrameGrabber -> IO ()
        loop vfg = do
          img <- withUsbIOMutex $ nextFrame vfg
          let imgBytes = JPG.encodeJpegAtQuality 80 img
          imgBytes `deepseq` writeIORef imgBytesRef imgBytes
          threadDelay delayMicros
          loop vfg

        delayMicros = refreshIntervalSecs * 1000000


-- stupid hack to query multiple attached USB cams without exhausting
-- driver resources
usbIOMutex :: MVar ()
usbIOMutex = unsafePerformIO $ newMVar ()
{-# NOINLINE usbIOMutex  #-}

withUsbIOMutex :: IO a -> IO a
withUsbIOMutex act = bracket (takeMVar usbIOMutex)
                             (putMVar usbIOMutex )
                             (\_ -> act)


err :: String -> IO ()
err msg =  putStrLn $ "ERROR (Webcam): " ++ msg

warn :: String -> IO ()
warn msg = putStrLn $ "WARN  (Webcam): " ++ msg

info :: String -> IO ()
info msg = putStrLn $ "INFO  (Webcam): " ++ msg
