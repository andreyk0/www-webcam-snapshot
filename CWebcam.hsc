{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module CWebcam (
    V4L2Device
  , V4L2FrameGrabber
  , hasVideoStreamingCapability
  , nextFrame
  , withFrameGrabber
  , withV4L2Device
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Monoid
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.IO.Select
import System.Posix.IO.Select.Types
import System.Posix.Types

import qualified Codec.Picture as CP

#include <fcntl.h>
#include <libv4l2.h>
#include <linux/videodev2.h>
#include <stddef.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

data V4L2Device = V4L2Device { v4l2FilePath :: String
                             , v4l2Fd :: CInt
                             } deriving (Eq, Show)

data V4L2FrameGrabber = V4L2FrameGrabber { v4l2Dev :: V4L2Device
                                         , imgData :: Ptr Word8
                                         } deriving (Eq, Show)

-- extern int ioctl (int __fd, unsigned long int __request, ...) __THROW;
foreign import ccall unsafe "ioctl" c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

--LIBV4L_PUBLIC int v4l2_open(const char *file, int oflag, ...);
foreign import ccall unsafe "v4l2_open" c_v4l2_open :: CString -> CInt -> CInt -> IO CInt

--LIBV4L_PUBLIC int v4l2_close(int fd);
foreign import ccall unsafe "v4l2_close" c_v4l2_close :: CInt -> IO ()

-- LIBV4L_PUBLIC void *v4l2_mmap(void *start, size_t length, int prot, int flags, int fd, int64_t offset);
foreign import ccall unsafe "v4l2_mmap" c_v4l2_mmap :: Ptr () -> CULong -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

-- LIBV4L_PUBLIC int v4l2_munmap(void *_start, size_t length);
foreign import ccall unsafe "v4l2_munmap" c_v4l2_munmap :: Ptr a -> CULong -> IO CInt

-- void *memset(void *s, int c, size_t n);
foreign import ccall unsafe "memset" c_memset :: Ptr a -> CInt -> CULong -> IO (Ptr ())


memclear :: Ptr a -> CULong -> IO ()
memclear ptr sz = do
  _ <- c_memset ptr 0 sz
  return ()


-- int xioctl(int fd, int request, void *arg)
xioctl :: V4L2Device -> CULong -> Ptr a -> IO ()
xioctl V4L2Device{..} req arg = runInBoundThread $ do
  throwErrnoIfRetry_ (<0) v4l2FilePath $ c_ioctl v4l2Fd req arg


--c_v4l2_mmap :: Ptr () -> CULong -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)
v4l2Mmap :: CInt -> CULong -> CLong -> IO (Ptr Word8)
v4l2Mmap fd len offset = c_v4l2_mmap nullPtr len #{const PROT_READ | PROT_WRITE} #{const MAP_SHARED} fd offset

-- | Device resource management
withV4L2Device :: FilePath -- ^ device path, e.g. /dev/videoX
               -> (V4L2Device -> IO a) -- ^ device handler function
               -> IO (Either IOException a) -- ^ IO exception or computed result
withV4L2Device fp handler = runInBoundThread . try $
  withCString fp $ \cfp -> do

    fd <- throwErrnoIf (<0) fp $
      c_v4l2_open cfp (#{const O_RDWR | O_NONBLOCK}) 0

    (handler (V4L2Device fp fd))
      `finally` (c_v4l2_close fd)


-- | Checks if device has a video streaming capability (required)
hasVideoStreamingCapability :: V4L2Device -> IO Bool
hasVideoStreamingCapability vd@V4L2Device{..} = do
  allocaBytes #{size struct v4l2_capability} $ \cap -> do
    xioctl vd #{const VIDIOC_QUERYCAP} cap
    capabilities :: CUInt <- #{peek struct v4l2_capability, capabilities} cap
    let videoCapture = capabilities .&. #{const V4L2_CAP_VIDEO_CAPTURE} /= 0
    let streaming    = capabilities .&. #{const V4L2_CAP_STREAMING}     /= 0
    return $ videoCapture && streaming


-- | Witdth of the captured image
imageWidth :: CUInt
imageWidth = 640

-- | Height of the captured image
imageHeight :: CUInt
imageHeight = 480


-- | Configures video device and executes provided handler function
withFrameGrabber :: V4L2Device -- ^ video device to use (must have streaming capability)
                 -> (V4L2FrameGrabber -> IO a) -- ^ handler to execute
                 -> IO a -- computed handler result
withFrameGrabber vd handleFG = do
  vcap <- hasVideoStreamingCapability vd
  return $ assert vcap ()

  setImageFormat vd

  withImageBytes vd $ \imgBytes ->
    handleFG $ V4L2FrameGrabber vd imgBytes


-- | Reads next frame from the video device and decodes to juicypixels Image
nextFrame :: V4L2FrameGrabber -- ^ frame grabber device
          -> IO (CP.Image CP.PixelYCbCr8) -- ^ captured and decoded image frame
nextFrame V4L2FrameGrabber{..} = do
  grabFrame v4l2Dev 5
  decodeImage imgData


decodeImage :: Ptr Word8 -> IO (CP.Image CP.PixelYCbCr8)
decodeImage imgBytes =
  CP.withImage (fromIntegral imageWidth)
               (fromIntegral imageHeight)
               (decodePixel imgBytes)


-- http://linuxtv.org/downloads/v4l-dvb-apis/V4L2-PIX-FMT-YUYV.html
decodePixel :: Ptr Word8 -> Int -> Int -> IO CP.PixelYCbCr8
decodePixel imgBytes x y = do
  let pixelOffset = y*(fromIntegral imageWidth) + x
  let baseOffset = pixelOffset .&. (complement 1)

  cy <- peekByteOff imgBytes (pixelOffset * 2)
  cb <- peekByteOff imgBytes (baseOffset * 2 + 1)
  cr <- peekByteOff imgBytes (baseOffset * 2 + 3)

  return $ CP.PixelYCbCr8 cy cb cr


-- 640x480 YUYV -- should be fairly common
-- http://linuxtv.org/downloads/v4l-dvb-apis/V4L2-PIX-FMT-YUYV.html
setImageFormat :: V4L2Device -> IO ()
setImageFormat vd@V4L2Device{..} = do
  allocaBytes #{size struct v4l2_format} $ \fmt -> do
    memclear fmt #{size struct v4l2_format}
    #{poke struct v4l2_format, type}                 fmt (#{const V4L2_BUF_TYPE_VIDEO_CAPTURE} :: CUInt)
    #{poke struct v4l2_format, fmt.pix.width}        fmt imageWidth
    #{poke struct v4l2_format, fmt.pix.height}       fmt imageHeight
    #{poke struct v4l2_format, fmt.pix.field}        fmt (#{const V4L2_FIELD_NONE} :: CUInt)
    #{poke struct v4l2_format, fmt.pix.pixelformat}  fmt (#{const V4L2_PIX_FMT_YUYV} :: CUInt)

    xioctl vd #{const VIDIOC_S_FMT} fmt

    fmtW    :: CUInt <- #{peek struct v4l2_format, fmt.pix.width}        fmt
    fmtH    :: CUInt <- #{peek struct v4l2_format, fmt.pix.height}       fmt
    fmtPf   :: CUInt <- #{peek struct v4l2_format, fmt.pix.pixelformat}  fmt

    let fmtPfCh :: Ptr CChar = #{ptr struct v4l2_format, fmt.pix.pixelformat}  fmt

    pfStr <- peekCStringLen (fmtPfCh, 4) -- 4 chars in a C Int
    putStrLn $ show vd <> " driver selected " <> pfStr <> " w" <> show fmtW <> " h" <> show fmtH

    -- make sure driver accepted our settings
    return $ assert (fmtW == imageWidth)
             assert (fmtH == imageHeight)
             assert (fmtPf == #{const V4L2_PIX_FMT_YUYV})
             ()


withImageBytes :: V4L2Device -> (Ptr Word8 -> IO a) -> IO a
withImageBytes vd@V4L2Device{..} handleImageBytes = do
  allocaBytes #{size struct v4l2_requestbuffers} $ \req -> do
    #{poke struct v4l2_requestbuffers, count}  req (1 :: CInt)
    #{poke struct v4l2_requestbuffers, type}   req (#{const V4L2_BUF_TYPE_VIDEO_CAPTURE} :: CInt)
    #{poke struct v4l2_requestbuffers, memory} req (#{const V4L2_MEMORY_MMAP} :: CInt)
    xioctl vd #{const VIDIOC_REQBUFS} req

    allocaBytes #{size struct v4l2_buffer} $ \buf -> do
      #{poke struct v4l2_buffer, type}   buf (#{const V4L2_BUF_TYPE_VIDEO_CAPTURE} :: CInt)
      #{poke struct v4l2_buffer, memory} buf (#{const V4L2_MEMORY_MMAP} :: CInt)
      #{poke struct v4l2_buffer, index}  buf (0 :: CInt)
      xioctl vd #{const VIDIOC_QUERYBUF} buf
      len :: CUInt        <- #{peek struct v4l2_buffer, length} buf
      offset :: CUInt     <- #{peek struct v4l2_buffer, m.offset} buf
      bytesUsed :: CUInt  <- #{peek struct v4l2_buffer, bytesused} buf

      putStrLn $ "DEV " <> show vd <> " BUF " <> show len <> " OFF " <> show offset <> " BYTES USED " <> show bytesUsed
      -- http://linuxtv.org/downloads/v4l-dvb-apis/V4L2-PIX-FMT-YUYV.html
      assert (len == imageHeight * imageWidth * 2) $ return ()
      assert (offset == 0) $ return () -- seems to be the case with current settings, for now avoid extra param

      bracket (v4l2Mmap v4l2Fd (fromIntegral len) (fromIntegral offset))
              (\ptr -> c_v4l2_munmap ptr (fromIntegral len))
              handleImageBytes


-- | Stream for a little while, grab a frame, optionally skipping a few first (usually junk)
grabFrame :: V4L2Device -- ^ device to grab frame from
          -> Int -- ^ number of frames to skip
          -> IO ()
grabFrame vd@V4L2Device{..} skipCount = do
  allocaBytes #{size struct v4l2_buffer} $ \buf -> do
    memclear buf #{size struct v4l2_buffer}
    #{poke struct v4l2_buffer, type}   buf (#{const V4L2_BUF_TYPE_VIDEO_CAPTURE} :: CInt)
    #{poke struct v4l2_buffer, memory} buf (#{const V4L2_MEMORY_MMAP} :: CInt)
    #{poke struct v4l2_buffer, index}  buf (0 :: CInt)

    -- query buffer
    xioctl vd #{const VIDIOC_QBUF} buf

    -- start streaming
    xioctl vd #{const VIDIOC_STREAMON} $ #{ptr struct v4l2_buffer, type} buf

    -- skip a few junk frames
    forM_ [1 ..skipCount] $ \_ -> do
      throwErrnoIf_ (<0) v4l2FilePath $ select'' [Fd v4l2Fd] [] [] (finite 1 0)
      xioctl vd #{const VIDIOC_DQBUF} buf
      xioctl vd #{const VIDIOC_QBUF} buf

    -- grab 2nd frame
    throwErrnoIf_ (<0) v4l2FilePath $ select'' [Fd v4l2Fd] [] [] (finite 1 0)
    xioctl vd #{const VIDIOC_DQBUF} buf

    -- stop streaming
    xioctl vd #{const VIDIOC_STREAMOFF} $ #{ptr struct v4l2_buffer, type} buf
