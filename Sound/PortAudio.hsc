{-# LANGUAGE BangPatterns,
             DeriveDataTypeable,
             EmptyDataDecls,
             ForeignFunctionInterface,
             GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}


------------------------------------------------------------------------------
--
-- | Module:     Sound.PortAudio
-- Copyright:    (c) 2009 Mietek Bak
-- License:      MIT
--               
-- Maintainer:   mietek@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (GHC extensions)
--
-- TODO


module Sound.PortAudio (
  -- * Library
  -- ** Library lifecycle
  initialize,
  terminate,
  withPortAudio,
  -- ** Version information
  getVersion,
  getVersionText,
  -- * Streams
  Stream,
  -- ** Stream lifecycle
  openDefaultStream,
  openStream,
  closeStream,
  startStream,
  stopStream,
  abortStream,
  withDefaultStream,
  withStream,
  setStreamFinishedCallback,
  ChannelCount,
  SampleRate,
  FrameCount,
  StreamCallback,
  StreamCallbackTimeInfo (..),
  StreamCallbackFlags,
  StreamCallbackFlag (..),
  StreamCallbackResult (..),
  StreamParameters (..),
  StreamFlags,
  StreamFlag (..),
  StreamFinishedCallback,
  -- ** Blocking stream access
  readStream,
  writeStream,
  getStreamReadAvailable,
  getStreamWriteAvailable,
  -- ** Stream information
  isStreamStopped,
  isStreamActive,
  getStreamInfo,
  getStreamTime,
  getStreamCpuLoad,
  StreamInfo (..),
  -- ** Sample format information
  assertSampleFormatSupported,
  getSampleSize,
  SampleFormat,
  SampleFormatFlag (..),
  SampleSize,
  -- * Devices
  -- ** Device enumeration
  getDeviceCount,
  getDefaultInputDeviceIndex,
  getDefaultOutputDeviceIndex,
  DeviceCount,
  DeviceIndex,
  -- ** Device information
  getDeviceInfo,
  DeviceInfo (..),
  -- * Host APIs
  HostApi (..),
  -- ** Host API enumeration
  getHostApiCount,
  getDefaultHostApiIndex,
  getHostApiIndex,
  HostApiCount,
  HostApiIndex,
  -- ** Host API information
  getHostApiInfo,
  HostApiInfo (..),
  -- ** Host API device enumeration
  hostApiDeviceIndexToDeviceIndex,
  HostApiDeviceCount,
  HostApiDeviceIndex,
  -- ** Host error information
  getLastHostErrorInfo,
  HostErrorInfo (..),
  HostErrorCode,
  -- * Exceptions
  PortAudioException (..)
) where

import Control.Exception (Exception, bracket, bracket_, finally, onException,
                          throw)
import Control.Monad (liftM, when)
import Data.Bits (Bits, (.&.), (.|.))
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.List (foldl')
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble, CInt, CLong, CULong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr, nullFunPtr,
                    plusPtr)
import Foreign.Storable (Storable, alignment, peek, poke, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

#include <portaudio.h>


------------------------------------------------------------------------------
--
-- * Library


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Library lifecycle


-- | Initializes internal data structures and prepares underlying host APIs
-- for use.  With the exception of 'getVersion' and 'getVersionText', this
-- function /must/ be called before using any other PortAudio API functions.
--
-- If 'initialize' is called multiple times, each successful call must be
-- matched with a corresponding call to 'terminate'.  Pairs of calls to
-- 'initialize' and 'terminate' may overlap, and are not required to be fully
-- nested.  Consider using 'withPortAudio' or 'withDefaultStream' instead.
--
-- Throws a 'PortAudioException' if an error occurs.  Should this happen,
-- 'terminate' should /not/ be called.

initialize :: IO ()
initialize = liftM' asUnit initialize_

foreign import ccall "portaudio.h Pa_Initialize"
  initialize_ :: IO UnitOrError_


-- | Deallocates all resources allocated by PortAudio since it was initialized
-- by a call to 'initialize'.  In cases where 'initialize' has been called
-- multiple times, each call must be matched with a corresponding call to
-- 'terminate'.  The final matching call to 'terminate' will automatically
-- close any PortAudio streams that are still open.
--
-- 'terminate' /must/ be called before exiting a program which uses PortAudio.
-- Failure to do so may result in serious resource leaks, such as audio
-- devices not being available until the next reboot.  Consider using
-- 'withPortAudio' or 'withDefaultStream' instead.
--
-- Throws a 'PortAudioException' if an error occurs. 

terminate :: IO ()
terminate = liftM' asUnit terminate_

foreign import ccall "portaudio.h Pa_Terminate"
  terminate_ :: IO UnitOrError_


-- | Initializes PortAudio, executes the computation passed within the context
-- of the library, then terminates PortAudio.
--
-- Throws a 'PortAudioException' if an error occurs.

withPortAudio ::
     IO a
     -- ^ A computation to be executed.
  -> IO a
withPortAudio = bracket_ initialize terminate


newtype UnitOrError_ = UnitOrError_ CInt
  deriving (Eq, Show)

asUnit :: UnitOrError_ -> ()
asUnit (UnitOrError_ n)
  | n == (#const paNoError) = ()
  | otherwise               = throwAsError n


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Version information


getVersion :: Int
getVersion = fromIntegral getVersion_
-- ^ Retrieves the release number of the currently running PortAudio build,
-- e.g. @1900@.

foreign import ccall "portaudio.h Pa_GetVersion"
  getVersion_ :: CInt


getVersionText :: String
getVersionText = unsafePerformIO (peekCString getVersionText_)
-- ^ Retrieves a textual description of the current PortAudio build, e.g.
-- @\"PortAudio V19-devel 13 October 2002\"@.

foreign import ccall "portaudio.h Pa_GetVersionText"
  getVersionText_ :: CString


------------------------------------------------------------------------------
--
-- * Streams


-- | A single 'Stream' can provide multiple channels of real-time streaming
-- audio input and output to a client application.  A stream provides access
-- to audio hardware represented by one or more devices.
--
-- Depending on the underlying host API, it may be possible to open multiple
-- streams using the same device, however this behavior is
-- implementation-defined.  Portable applications should assume that a single
-- device may be simultaneously used by at most one stream.

data Stream a b =
  Stream
    !(Stream_ a b)
    !(IORef (StreamCallbackPtr_ a b))
    !(IORef StreamFinishedCallbackPtr_)
      deriving (Eq)

newtype Stream_ a b = Stream_ (Ptr Stream__)
  deriving (Eq, Show, Storable)

data Stream__

instance Show (Stream a b) where
  show (Stream (Stream_ ptr) _ _) = "Stream (" ++ show ptr ++ ")"

fromStream :: Stream a b -> Stream_ a b
fromStream (Stream stm_ _ _) = stm_

mkStream :: Ptr (Stream_ a b) -> StreamCallbackPtr_ a b -> IO (Stream a b)
mkStream stmPtr cb_ = do
  stm_   <- peek stmPtr
  cbRef  <- newIORef cb_
  fCbRef <- newIORef nullFunPtr
  return (Stream stm_ cbRef fCbRef)


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Stream lifecycle


-- | Opens a stream using the default input and/or output devices for either
-- input, output, or both.  The returned stream is inactive (stopped).
--
-- Consider using 'withDefaultStream' instead.
--
-- Throws a 'PortAudioException' if an error occurs.

openDefaultStream ::
     ChannelCount
     -- ^ The number of channels of sound that will be supplied to the stream
     -- callback or returned by 'readStream'.  It can range from @1@ to the
     -- value of 'diMaxInputChannelCount'.  @0@ may be used for output-only
     -- streams.
  -> ChannelCount
     -- ^ The number of channels of sounds to be delivered to the stream
     -- callback or passed to 'writeStream'.  It can range from @1@ to the
     -- value of 'diMaxOutputChannelCount'.  @0@ may be used for input-only
     -- streams.
  -> SampleFormat
     -- ^ The sample format of both the input and output buffers provided to
     -- the callback or passed to and from 'readStream' and 'writeStream'.
  -> SampleRate
     -- ^ The desired sample rate. For full-duplex streams it is the sample
     -- rate for both input and output.
  -> FrameCount
     -- ^ The number of frames passed to the stream callback, or the preferred
     -- block granularity for a blocking read/write stream.  @0@ may be used
     -- to request that the stream callback will receive an optimal (and
     -- possibly varying) number of frames based on host requirements and the
     -- requested latency settings.
     --
     -- Note that with some host APIs the use of a specific number of frames
     -- per buffer for a callback stream may introduce an additional layer of
     -- buffering, which could introduce additional latency.  PortAudio
     -- guarantees that the additional latency will be kept to the theoretical
     -- minimum, however it is strongly recommended that a specific number of
     -- frames per buffer only be used when your algorithm requires a fixed
     -- number of frames per stream callback.
  -> Maybe (StreamCallback a b)
     -- ^ A client-supplied function responsible for processing and filling
     -- input and output buffers.  'Nothing' may be used to open the stream in
     -- blocking read/write mode.
     --
     -- In blocking mode, the client can receive sample data using
     -- 'readStream' and write sample data using 'writeStream'; the number of
     -- samples that may be read or written without blocking is returned by
     -- 'getStreamReadAvailable' and 'getStreamWriteAvailable' respectively.
  -> IO (Stream a b)
openDefaultStream inChs outChs fmt rt fms cb =
  alloca $ \ stmPtr -> do
    cb_ <- mkStreamCallback cb
    onException (liftM' asUnit (openDefaultStream_ stmPtr inChs_ outChs_ fmt_
                                                   rt_ fms_ cb_ nullPtr))
                (maybeFreeFunPtr cb_)
    mkStream stmPtr cb_
      where inChs_  = fromChannelCount inChs
            outChs_ = fromChannelCount outChs
            fmt_    = fromSampleFormat fmt
            rt_     = fromSampleRate rt
            fms_    = fromFrameCount fms

foreign import ccall "portaudio.h Pa_OpenDefaultStream"
  openDefaultStream_ ::
       Ptr (Stream_ a b)
    -> ChannelCount_
    -> ChannelCount_
    -> SampleFormat_
    -> SampleRate_
    -> FrameCount_
    -> StreamCallbackPtr_ a b
    -> Ptr ()
    -> IO UnitOrError_


-- | A more general version of 'openDefaultStream'.  The returned stream is
-- inactive (stopped).
--
-- Throws a 'PortAudioException' if an error occurs.

openStream ::
     Maybe StreamParameters
     -- ^ A structure describing the input parameters used by the opened
     -- stream.  'Nothing' may be used for output-only streams.
  -> Maybe StreamParameters
     -- ^ A structure describing the output parameters used by the opened
     -- stream.  'Nothing' may be used for input-only streams.
  -> SampleRate
     -- ^ See 'openDefaultStream' for further details.
  -> FrameCount
     -- ^ Ditto.
  -> Maybe (StreamCallback a b)
     -- ^ Ditto.
  -> StreamFlags
     -- ^ Flags which modify the behavior of the streaming process.  Some
     -- flags may only be relevant to certain buffer formats.
  -> IO (Stream a b)
openStream inPrms outPrms rt fms cb flgs =
  maybeWith inPrms $ \ inPrmsPtr ->
    maybeWith outPrms $ \ outPrmsPtr ->
      alloca $ \ stmPtr -> do
        cb_ <- mkStreamCallback cb
        onException
          (liftM' asUnit (openStream_ stmPtr inPrmsPtr outPrmsPtr rt_ fms_
                                      flgs_ cb_ nullPtr))
          (maybeFreeFunPtr cb_)
        mkStream stmPtr cb_
          where rt_   = fromSampleRate rt
                fms_ = fromFrameCount fms
                flgs_ = fromStreamFlags flgs

foreign import ccall "portaudio.h Pa_OpenStream"
  openStream_ ::
       Ptr (Stream_ a b)
    -> Ptr StreamParameters
    -> Ptr StreamParameters
    -> SampleRate_
    -> FrameCount_
    -> StreamFlags_
    -> StreamCallbackPtr_ a b
    -> Ptr ()
    -> IO UnitOrError_


-- | Closes an audio stream.  If the stream is active, it discards any pending
-- buffers as if 'abortStream' had been called.
--
-- 'closeStream' /must/ be called for each opened stream to avoid leaking
-- memory in the Haskell binding.  Consider using 'withDefaultStream' instead.
--
-- Throws a 'PortAudioException' if an error occurs.

closeStream :: Stream a b -> IO ()
closeStream (Stream stm_ cbRef fCbRef) =
  finally (liftM' asUnit (closeStream_ stm_)) $ do
    cb_ <- atomicModifyIORef cbRef ((,) nullFunPtr)
    maybeFreeFunPtr cb_
    fCb_ <- atomicModifyIORef fCbRef ((,) nullFunPtr)
    maybeFreeFunPtr fCb_

foreign import ccall "portaudio.h Pa_CloseStream"
  closeStream_ :: Stream_ a b -> IO UnitOrError_


-- | Commences audio processing.
--
-- Consider using 'withStream' instead.
--
-- Throws a 'PortAudioException' if an error occurs.

startStream :: Stream a b -> IO ()
startStream stm = liftM' asUnit (startStream_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_StartStream"
  startStream_ :: Stream_ a b -> IO UnitOrError_


-- | Terminates audio processing.  This function waits until all pending audio
-- buffers have been played before it returns.
--
-- Consider using 'withStream' instead.
--
-- Throws a 'PortAudioException' if an error occurs.

stopStream :: Stream a b -> IO ()
stopStream stm = liftM' asUnit (stopStream_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_StopStream"
  stopStream_ :: Stream_ a b -> IO UnitOrError_


-- | Terminates audio processing immediately, without waiting for pending
-- buffers to complete.
--
-- Throws a 'PortAudioException' if an error occurs.

abortStream :: Stream a b -> IO ()
abortStream stm = liftM' asUnit (abortStream_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_AbortStream"
  abortStream_ :: Stream_ a b -> IO UnitOrError_


-- | Initializes PortAudio, opens and starts a new default stream, executes
-- the computation passed within the context of the stream, stops and closes
-- the stream, then terminates PortAudio.
--
-- Throws a 'PortAudioException' if an error occurs.

withDefaultStream ::
     ChannelCount
     -- ^ See 'openDefaultStream' for further details.
  -> ChannelCount
     -- ^ Ditto.
  -> SampleFormat
     -- ^ Ditto.
  -> SampleRate
     -- ^ Ditto.
  -> FrameCount
     -- ^ Ditto.
  -> Maybe (StreamCallback a b)
     -- ^ Ditto.
  -> (Stream a b -> IO c)
     -- ^ A computation to be executed.
  -> IO c
withDefaultStream inChs outChs fmt rt fms cb fun =
  withPortAudio $
    bracket (openDefaultStream inChs outChs fmt rt fms cb) closeStream
            (\ stm -> withStream stm (fun stm))


-- | Starts the passed stream, executes the computation passed within the
-- context of the stream, then stops the stream.
--
-- Throws a 'PortAudioException' if an error occurs.

withStream ::
     Stream a b
     -- ^ An open stream.
  -> IO c
     -- ^ A computation to be executed.
  -> IO c
withStream stm = bracket_ (startStream stm) (stopStream stm)


-- | Registers a stream finished callback, which will be called when the
-- stream becomes inactive.  See 'StreamFinishedCallback' for further details.
--
-- Throws a 'PortAudioException' if an error occurs.

setStreamFinishedCallback ::
     Stream a b
     -- ^ TODO
  -> Maybe StreamFinishedCallback
     -- ^ TODO
  -> IO ()
setStreamFinishedCallback (Stream stm_ _ fCbRef) fCb = do
  newFCb_ <- mkStreamFinishedCallback fCb
  onException (liftM' asUnit (setStreamFinishedCallback_ stm_ newFCb_))
              (maybeFreeFunPtr newFCb_)
  oldFCb_ <- atomicModifyIORef fCbRef ((,) newFCb_)
  maybeFreeFunPtr oldFCb_

foreign import ccall "portaudio.h Pa_SetStreamFinishedCallback"
  setStreamFinishedCallback_ ::
       Stream_ a b
    -> StreamFinishedCallbackPtr_
    -> IO UnitOrError_


-- | TODO

type ChannelCount = Int

newtype ChannelCount_ = ChannelCount_ CInt
  deriving (Eq, Show, Storable)

fromChannelCount :: ChannelCount -> ChannelCount_
fromChannelCount = ChannelCount_ . fromIntegral

toChannelCount :: ChannelCount_ -> ChannelCount
toChannelCount (ChannelCount_ n) = fromIntegral n


-- | TODO

type SampleRate = Double

newtype SampleRate_ = SampleRate_ CDouble
  deriving (Eq, Show, Storable)

fromSampleRate :: SampleRate -> SampleRate_
fromSampleRate = SampleRate_ . realToFrac

toSampleRate :: SampleRate_ -> SampleRate
toSampleRate (SampleRate_ r) = realToFrac r


-- | TODO

type FrameCount = Int

newtype FrameCount_ = FrameCount_ CULong
  deriving (Eq, Show)

newtype FrameCountOrError_ = FrameCountOrError_ CULong
  deriving (Eq, Show)

fromFrameCount :: FrameCount -> FrameCount_
fromFrameCount = FrameCount_ . fromIntegral

toFrameCount :: FrameCount_ -> FrameCount
toFrameCount (FrameCount_ n) = fromIntegral n

asFrameCount :: FrameCountOrError_ -> FrameCount
asFrameCount (FrameCountOrError_ n)
  | n >= (#const paNoError) = fromIntegral n
  | otherwise               = throwAsError n


-- | Functions of type 'StreamCallback' are implemented by PortAudio clients.
-- They consume, process or generate audio in response to requests from an
-- active PortAudio stream.
--
-- With the exception of 'getStreamCpuLoad' it is not permitted to call
-- PortAudio API functions from within the stream callback.
--
-- TODO: figure out how to get Haddock to document portions of type synonym
-- declarations.

type StreamCallback a b =
     Ptr a
  -> Ptr b
  -> FrameCount
  -> Maybe StreamCallbackTimeInfo
  -> StreamCallbackFlags
  -> IO StreamCallbackResult

type StreamCallback_ a b =
     Ptr a
  -> Ptr b
  -> FrameCount_
  -> Ptr StreamCallbackTimeInfo
  -> StreamCallbackFlags_
  -> Ptr ()
  -> IO StreamCallbackResult_

type StreamCallbackPtr_ a b = FunPtr (StreamCallback_ a b)

callStreamCallback :: StreamCallback a b -> StreamCallback_ a b
callStreamCallback cb inPtr outPtr fms_ tm_ flgs_ _ = do
  tm <- maybePeek' tm_
  liftM' fromStreamCallbackResult (cb inPtr outPtr fms tm flgs)
  where fms  = toFrameCount fms_
        flgs = toStreamCallbackFlags flgs_

mkStreamCallback :: Maybe (StreamCallback a b) -> IO (StreamCallbackPtr_ a b)
mkStreamCallback Nothing   = return nullFunPtr
mkStreamCallback (Just cb) = mkStreamCallback_ (callStreamCallback cb)

foreign import ccall "wrapper"
  mkStreamCallback_ :: StreamCallback_ a b -> IO (StreamCallbackPtr_ a b)


-- | TODO

data StreamCallbackTimeInfo =
  StreamCallbackTimeInfo {
    sctiInputBufferAdcTime  :: !Double,
    -- ^ TODO
    sctiCurrentTime         :: !Double,
    -- ^ TODO
    sctiOutputBufferDacTime :: !Double
    -- ^ TODO
  } deriving (Eq, Show)

instance Storable StreamCallbackTimeInfo where
  sizeOf _    = (#size PaStreamCallbackTimeInfo)
  alignment _ = alignment nullPtr
  peek ptr    = do
    in_  :: CDouble <- peekAt ptr (#offset PaStreamCallbackTimeInfo, inputBufferAdcTime)
    cur_ :: CDouble <- peekAt ptr (#offset PaStreamCallbackTimeInfo, currentTime)
    out_ :: CDouble <- peekAt ptr (#offset PaStreamCallbackTimeInfo, outputBufferDacTime)
    return $ StreamCallbackTimeInfo {
      sctiInputBufferAdcTime  = realToFrac in_,
      sctiCurrentTime         = realToFrac cur_,
      sctiOutputBufferDacTime = realToFrac out_
    }


-- | TODO

type StreamCallbackFlags = [StreamCallbackFlag]

newtype StreamCallbackFlags_ = StreamCallbackFlags_ CULong
  deriving (Eq, Show)

-- TODO: figure out why enumFromTo is not helpful.
toStreamCallbackFlags :: StreamCallbackFlags_ -> StreamCallbackFlags
toStreamCallbackFlags (StreamCallbackFlags_ n) =
  filter (matchFlag n) [InputUnderflow, InputOverflow, OutputUnderflow,
                        OutputOverflow, PrimingOutput]


-- | TODO

data StreamCallbackFlag =
    InputUnderflow
    -- ^ In a stream opened with an unspecified number of frames per buffer,
    -- indicates that input data is all silence (zeroes) because no real data
    -- is available.  Otherwise, indicates that one or more zero samples have
    -- been inserted into the input buffer to compensate for an input
    -- underflow.
  | InputOverflow
    -- ^ In a stream opened with an unspecified number of frames per buffer,
    -- indicates that data prior to the first sample of the input buffer was
    -- discarded due to an overflow, possibly because the stream callback is
    -- using too much CPU time.  Otherwise, indicates that data prior to one
    -- or more samples in the input buffer was discarded.
  | OutputUnderflow
    -- ^ Indicates that output data (or a gap) was inserted, possibly because
    -- the stream callback is using too much CPU time.
  | OutputOverflow
    -- ^ Indicates that output data will be discarded because no room is
    -- available.
  | PrimingOutput
    -- ^ Some of all of the output data will be used to prime the stream;
    -- input data may be zero.
      deriving (Eq, Show)

instance Enum StreamCallbackFlag where
  fromEnum e = case e of
    InputUnderflow             -> (#const paInputUnderflow)
    InputOverflow              -> (#const paInputOverflow)
    OutputUnderflow            -> (#const paOutputUnderflow)
    OutputOverflow             -> (#const paOutputOverflow)
    PrimingOutput              -> (#const paPrimingOutput)
  toEnum n = case n of
    (#const paInputUnderflow)  -> InputUnderflow
    (#const paInputOverflow)   -> InputOverflow
    (#const paOutputUnderflow) -> OutputUnderflow
    (#const paOutputOverflow)  -> OutputOverflow
    (#const paPrimingOutput)   -> PrimingOutput
    _ -> error ("unknown stream callback flag: " ++ show n)


-- | Allowed return values for the 'StreamCallback'.

data StreamCallbackResult =
    Continue
    -- ^ TODO
  | Complete
    -- ^ TODO
  | Abort
    -- ^ TODO
      deriving (Eq, Show)

newtype StreamCallbackResult_ = StreamCallbackResult_ CInt
  deriving (Eq, Show)

instance Enum StreamCallbackResult where
  fromEnum e = case e of
    Continue            -> (#const paContinue)
    Complete            -> (#const paComplete)
    Abort               -> (#const paAbort)
  toEnum n = case n of
    (#const paContinue) -> Continue
    (#const paComplete) -> Complete
    (#const paAbort)    -> Abort
    _ -> error ("unknown stream callback result: " ++ show n)

fromStreamCallbackResult :: StreamCallbackResult -> StreamCallbackResult_
fromStreamCallbackResult = StreamCallbackResult_ . fromIntegral . fromEnum


-- | TODO
--
-- The usage of host API-specific stream information is not supported in the
-- Haskell binding.

data StreamParameters =
  StreamParameters {
    spDeviceIndex      :: !DeviceIndex,
    -- ^ TODO
    spChannelCount     :: !ChannelCount,
    -- ^ TODO
    spSampleFormat     :: !SampleFormat,
    -- ^ TODO
    spSuggestedLatency :: !Double
    -- ^ TODO
  } deriving (Eq, Show)

instance Storable StreamParameters where
  sizeOf _      = (#size PaStreamParameters)
  alignment _   = alignment nullPtr
  poke ptr prms = do
    pokeAt ptr dev_    (#offset PaStreamParameters, device)
    pokeAt ptr chs_    (#offset PaStreamParameters, channelCount)
    pokeAt ptr fmt_    (#offset PaStreamParameters, sampleFormat)
    pokeAt ptr ltc_    (#offset PaStreamParameters, suggestedLatency)
    pokeAt ptr nullPtr (#offset PaStreamParameters, hostApiSpecificStreamInfo)
      where dev_ = fromDeviceIndex (spDeviceIndex prms)
            chs_ = fromChannelCount (spChannelCount prms)
            fmt_ = fromSampleFormat (spSampleFormat prms)
            ltc_ :: CDouble = realToFrac (spSuggestedLatency prms)


-- | TODO

type StreamFlags = [StreamFlag]

newtype StreamFlags_ = StreamFlags_ CULong
  deriving (Bits, Eq, Num, Show, Storable)

fromStreamFlags :: StreamFlags -> StreamFlags_
fromStreamFlags = StreamFlags_ . foldFlags


-- | Flags used to control the behavior of a stream.
--
-- Platform-specific flags are not supported in the Haskell binding.

data StreamFlag =
    ClipOff
    -- ^ Disables default clipping of out-of-range samples.
  | DitherOff
    -- ^ Disables default dithering.
  | NeverDropInput
    -- ^ Requests that where possible, a full-duplex stream will not discard
    -- overflowed input samples without calling the stream callback.  This
    -- flag is only valid for full-duplex callback streams with an unspecified
    -- number of frames per buffer.
  | PrimeOutputBuffers
    -- ^ Calls the stream callback to fill initial output buffers, rather than
    -- the default behavior of priming the buffers with silence (zeroes).
    -- This flag has no effect for input-only and blocking read/write streams.
      deriving (Eq, Show)

instance Enum StreamFlag where
  fromEnum e = case e of
    ClipOff                   -> (#const paClipOff)
    DitherOff                 -> (#const paDitherOff)
    NeverDropInput            -> (#const paNeverDropInput)
    PrimeOutputBuffers ->
      (#const paPrimeOutputBuffersUsingStreamCallback)
  toEnum n = case n of
    (#const paClipOff)        -> ClipOff
    (#const paDitherOff)      -> DitherOff
    (#const paNeverDropInput) -> NeverDropInput
    (#const paPrimeOutputBuffersUsingStreamCallback) ->
      PrimeOutputBuffers
    _ -> error ("unknown stream flag: " ++ show n)


-- | Functions of type 'StreamFinishedCallback' are implemented by PortAudio
-- clients.  They can be registered with a stream using
-- 'setStreamFinishedCallback'.  Once registered, they are called when the
-- stream becomes inactive, which is after the stream callback returns
-- 'Complete' or 'Abort', or when 'stopStream' or 'abortStream' is called.
--
-- For a stream providing audio output, if the stream callback returns
-- 'Complete' or 'stopStream' is called, the stream finished callback will not
-- be called until all generated sample data have been played.

type StreamFinishedCallback = IO ()

type StreamFinishedCallback_ = Ptr () -> IO ()

type StreamFinishedCallbackPtr_ = FunPtr StreamFinishedCallback_

callStreamFinishedCallback ::
     StreamFinishedCallback
  -> StreamFinishedCallback_
callStreamFinishedCallback cb _ = cb

mkStreamFinishedCallback ::
     Maybe StreamFinishedCallback
  -> IO StreamFinishedCallbackPtr_
mkStreamFinishedCallback Nothing   = return nullFunPtr
mkStreamFinishedCallback (Just cb) =
  mkStreamFinishedCallback_ (callStreamFinishedCallback cb)

foreign import ccall "wrapper"
  mkStreamFinishedCallback_ ::
       StreamFinishedCallback_
    -> IO StreamFinishedCallbackPtr_


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Blocking stream access


-- | Reads samples from an input stream.  This function doesn't return until
-- the entire buffer has been filled, which may involve waiting for the
-- operating system to supply the data.
--
-- Throws a 'PortAudioException' if an error occurs:
--
-- * The 'InputOverflowed' exception indicates that input data was discarded
-- by PortAudio after the previous call and before this call.

readStream ::
     Stream a b
     -- ^ An open stream.
  -> FrameCount
     -- ^ The number of frames to be read into the buffer.  The parameter is
     -- not constrained to a specific range, however high performance
     -- applications will want to match this parameter to the number of frames
     -- per buffer used when opening the stream.
  -> Ptr a
     -- ^ A pointer to a buffer of sample frames, structured according to the
     -- sample format and number of channels used when opening the stream.  If
     -- non-interleaved samples were requested, this should be a pointer to
     -- the first element of an array or non-interleaved buffer pointers, one
     -- for each channel.
  -> IO ()
readStream stm fms ptr = liftM' asUnit (readStream_ stm_ ptr fms_)
  where stm_  = fromStream stm
        fms_  = fromFrameCount fms

foreign import ccall "portaudio.h Pa_ReadStream"
  readStream_ :: Stream_ a b -> Ptr a -> FrameCount_ -> IO UnitOrError_


-- | Writes samples to an output stream.  This function doesn't return until
-- the entire buffer has been consumed, which may involve waiting for the
-- operating system to consume the data.
--
-- Throws a 'PortAudioException' if an error occurs:
--
-- * The 'OutputUnderflowed' exception indicates that additional output data
-- was inserted after the previous call and before this call.

writeStream ::
     Stream a b
     -- ^ TODO
  -> FrameCount
     -- ^ TODO
  -> Ptr b
     -- ^ TODO
  -> IO ()
writeStream stm fms ptr = liftM' asUnit (writeStream_ stm_ ptr fms_)
  where stm_  = fromStream stm
        fms_  = fromFrameCount fms

foreign import ccall "portaudio.h Pa_WriteStream"
  writeStream_ :: Stream_ a b -> Ptr b -> FrameCount_ -> IO UnitOrError_


-- | Retrieves the number of frames that can be read from the specified stream
-- without waiting.
--
-- Throws a 'PortAudioException' if an error occurs.

getStreamReadAvailable :: Stream a b -> IO FrameCount
getStreamReadAvailable stm =
  liftM' asFrameCount (getStreamReadAvailable_ stm_)
    where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_GetStreamReadAvailable"
  getStreamReadAvailable_ :: Stream_ a b -> IO FrameCountOrError_


-- | Retrieves the number of frames that can be written to the specified
-- stream without waiting.
--
-- Throws a 'PortAudioException' if an error occurs.

getStreamWriteAvailable :: Stream a b -> IO FrameCount
getStreamWriteAvailable stm =
  liftM' asFrameCount (getStreamWriteAvailable_ stm_)
    where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_GetStreamWriteAvailable"
  getStreamWriteAvailable_ :: Stream_ a b -> IO FrameCountOrError_


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Stream information


-- | Determines whether the stream is stopped.
--
-- A stream is considered to be stopped prior to a successful call to
-- 'startStream' and after a successful call to 'stopStream' or 'abortStream'.
-- If a stream callback returns a value other than 'Continue', the stream is
-- /not/ considered to be stopped.
--
-- Throws a 'PortAudioException' if an error occurs.

isStreamStopped :: Stream a b -> IO Bool
isStreamStopped stm = liftM' asBool (isStreamStopped_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_IsStreamStopped"
  isStreamStopped_ :: Stream_ a b -> IO BoolOrError_


-- | Determines whether the stream is active.
--
-- A stream is active after a successful call to 'startStream', until it
-- becomes inactive either as a result of a call to 'stopStream' or
-- 'abortStream', or as a result of a 'Complete' or 'Abort' return value from
-- the stream callback.  In the latter case, the stream is considered inactive
-- after the last buffer has finished playing.
--
-- Throws a 'PortAudioException' if an error occurs.

isStreamActive :: Stream a b -> IO Bool
isStreamActive stm = liftM' asBool (isStreamActive_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_IsStreamActive"
  isStreamActive_ :: Stream_ a b -> IO BoolOrError_


-- | Retrieves a structure containing information about the specified stream.
--
-- Throws a 'PortAudioException' if an error occurs.

getStreamInfo :: Stream a b -> IO (Maybe StreamInfo)
getStreamInfo stm = getStreamInfo_ stm_ >>= maybePeek'
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_GetStreamInfo"
  getStreamInfo_ :: Stream_ a b -> IO (Ptr StreamInfo)


-- | Determines the current time for the specified stream according to the
-- same clock used to generate buffer timestamps.  This time may be used for
-- synchronizing other events to the audio stream.
--
-- Returns @0.0@ if an error occurs.

getStreamTime :: Stream a b -> IO Double
getStreamTime stm = liftM' realToFrac (getStreamTime_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_GetStreamTime"
  getStreamTime_ :: Stream_ a b -> IO CDouble


-- | Retrieves CPU usage information for the specified stream.  This function
-- may be called from the stream callback.
--
-- The value returned represents the fraction of total CPU time consumed by a
-- callback stream's audio processing routines, including, but not limited to,
-- the client-supplied stream callback.  Values returned are usually between
-- @0.0@ and @1.0@, where @1.0@ indicates that the stream callback is
-- consuming the maximum number of CPU cycles possible to maintain real-time
-- operation.
--
-- Returns @0.0@ for blocking read/write streams, or if an error occurs.

getStreamCpuLoad :: Stream a b -> IO Double
getStreamCpuLoad stm = liftM' realToFrac (getStreamCpuLoad_ stm_)
  where stm_ = fromStream stm

foreign import ccall "portaudio.h Pa_GetStreamCpuLoad"
  getStreamCpuLoad_ :: Stream_ a b -> IO CDouble


-- | TODO

data StreamInfo =
  StreamInfo {
    siInputLatency  :: !Double,
    -- ^ TODO
    siOutputLatency :: !Double,
    -- ^ TODO
    siSampleRate    :: !SampleRate
    -- ^ TODO
  } deriving (Eq, Show)

instance Storable StreamInfo where
  sizeOf _    = (#size PaStreamInfo)
  alignment _ = alignment nullPtr
  peek ptr    = do
    vsn     :: CInt        <- peekAt ptr (#offset PaStreamInfo, structVersion)
    when (vsn /= 1) (error ("unknown stream info version: " ++ show vsn))
    inLtc_  :: CDouble     <- peekAt ptr (#offset PaStreamInfo, inputLatency)
    outLtc_ :: CDouble     <- peekAt ptr (#offset PaStreamInfo, outputLatency)
    rt_                    <- peekAt ptr (#offset PaStreamInfo, sampleRate)
    return $ StreamInfo {
      siInputLatency  = realToFrac inLtc_,
      siOutputLatency = realToFrac outLtc_,
      siSampleRate    = toSampleRate rt_
    }


newtype BoolOrError_ = BoolOrError_ CInt
  deriving (Eq, Show)

asBool :: BoolOrError_ -> Bool
asBool (BoolOrError_ n)
  | n == 0    = False
  | n == 1    = True
  | otherwise = throwAsError n


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Sample format information


-- | Determines whether it would be possible to open a stream with the
-- specified parameters.  The 'spSuggestedLatency' fields are ignored.
--
-- Throws a 'PortAudioException' if an error occurs.

assertSampleFormatSupported ::
     Maybe StreamParameters
     -- ^ TODO
  -> Maybe StreamParameters
     -- ^ TODO
  -> SampleRate
     -- ^ TODO
  -> IO ()
assertSampleFormatSupported inPms outPms rt =
  maybeWith inPms $ \ inPmsPtr ->
    maybeWith outPms $ \ outPmsPtr ->
      liftM' asUnit (isFormatSupported_ inPmsPtr outPmsPtr rt_)
        where rt_ = fromSampleRate rt

foreign import ccall "portaudio.h Pa_IsFormatSupported"
  isFormatSupported_ ::
       Ptr StreamParameters
    -> Ptr StreamParameters
    -> SampleRate_
    -> IO UnitOrError_


-- | Retrieves the size of a given sample format in bytes.
--
-- Throws a 'PortAudioException' if an error occurs:
--
-- * The 'SampleFormatNotSupported' exception indicates the specified format
-- is not supported.

getSampleSize :: SampleFormat -> IO SampleSize
getSampleSize fmt = liftM' asSampleSize (getSampleSize_ fmt_)
  where fmt_ = fromSampleFormat fmt

foreign import ccall "portaudio.h Pa_GetSampleSize"
  getSampleSize_ :: SampleFormat_ -> IO SampleSizeOrError_


-- | TODO

type SampleFormat = [SampleFormatFlag]

newtype SampleFormat_ = SampleFormat_ CULong
  deriving (Bits, Eq, Num, Show, Storable)

fromSampleFormat :: SampleFormat -> SampleFormat_
fromSampleFormat = SampleFormat_ . foldFlags


-- | Used to specify one or more sample formats.  Each value indicates a possible format for sound data passed to and from the stream callback, 'readStream' and 'writeStream'.
--
-- The standard formats 'Float32', 'Int16', 'Int32', 'Int24', 'Int8' and 'UInt8' are usually implemented by all implementations.
--
-- The floating point representation ('Float32') uses @+1.0@ and @-1.0@ as the maximum and minimum respectively.
--
-- 'UInt8' is an unsigned 8-bit format, where @128@ is considered \"ground\".
--
-- The 'NonInterleaved' flag indicates that a multichannel buffer is passed as a set of non-interleaved pointers.
--
-- The \"custom\" sample format flag is not supported in the Haskell binding.

data SampleFormatFlag =
    Float32
    -- ^ TODO
  | Int32
    -- ^ TODO
  | Int24
    -- ^ TODO
  | Int16
    -- ^ TODO
  | Int8
    -- ^ TODO
  | UInt8
    -- ^ TODO
  | NonInterleaved
    -- ^ TODO
      deriving (Eq, Show)

instance Enum SampleFormatFlag where
  fromEnum e = case e of
    Float32                   -> (#const paFloat32)
    Int32                     -> (#const paInt32)
    Int24                     -> (#const paInt24)
    Int16                     -> (#const paInt16)
    Int8                      -> (#const paInt8)
    UInt8                     -> (#const paUInt8)
    NonInterleaved            -> (#const paNonInterleaved)
  toEnum n = case n of
    (#const paFloat32)        -> Float32
    (#const paInt32)          -> Int32
    (#const paInt24)          -> Int24
    (#const paInt16)          -> Int16
    (#const paInt8)           -> Int8
    (#const paUInt8)          -> UInt8
    (#const paNonInterleaved) -> NonInterleaved
    _ -> error ("unknown sample format flag: " ++ show n)


-- | TODO

type SampleSize = Int

newtype SampleSizeOrError_ = SampleSizeOrError_ CInt
  deriving (Eq, Show)

asSampleSize :: SampleSizeOrError_ -> SampleSize
asSampleSize (SampleSizeOrError_ n)
  | n >= (#const paNoError) = fromIntegral n
  | otherwise               = throwAsError n


------------------------------------------------------------------------------
--
-- * Devices


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Device enumeration


-- | Retrieves the number of available devices, which may be @0@.
--
-- Throws a 'PortAudioException' if an error occurs.

getDeviceCount :: IO DeviceCount
getDeviceCount = liftM' asDeviceCount getDeviceCount_

foreign import ccall "portaudio.h Pa_GetDeviceCount"
  getDeviceCount_ :: IO DeviceCountOrError_


-- | Retrieves the index of the default input device.
--
-- Returns 'Nothing' if an error occurs.

getDefaultInputDeviceIndex :: IO (Maybe DeviceIndex)
getDefaultInputDeviceIndex =
  liftM' toMaybeDeviceIndex getDefaultInputDeviceIndex_

foreign import ccall "portaudio.h Pa_GetDefaultInputDevice"
  getDefaultInputDeviceIndex_ :: IO MaybeDeviceIndex_


-- | Retrieves the index of the default output device.
--
-- Returns 'Nothing' if an error occurs.

getDefaultOutputDeviceIndex :: IO (Maybe DeviceIndex)
getDefaultOutputDeviceIndex =
  liftM' toMaybeDeviceIndex getDefaultOutputDeviceIndex_

foreign import ccall "portaudio.h Pa_GetDefaultOutputDevice"
  getDefaultOutputDeviceIndex_ :: IO MaybeDeviceIndex_


-- | TODO

type DeviceCount = Int

newtype DeviceCountOrError_ = DeviceCountOrError_ CInt
  deriving (Eq, Show)

asDeviceCount :: DeviceCountOrError_ -> DeviceCount
asDeviceCount (DeviceCountOrError_ n)
  | n >= (#const paNoError) = fromIntegral n
  | otherwise               = throwAsError n


-- | Used to refer to audio devices.  Values of this type range from @0@ to
-- the result of 'getDeviceCount' minus @1@.

type DeviceIndex = Int

newtype DeviceIndex_ = DeviceIndex_ CInt
  deriving (Eq, Show, Storable)

newtype MaybeDeviceIndex_ = MaybeDeviceIndex_ CInt
  deriving (Eq, Show, Storable)

newtype DeviceIndexOrError_ = DeviceIndexOrError_ CInt
  deriving (Eq, Show)

fromDeviceIndex :: DeviceIndex -> DeviceIndex_
fromDeviceIndex = DeviceIndex_ . fromIntegral

asDeviceIndex :: DeviceIndexOrError_ -> DeviceIndex
asDeviceIndex (DeviceIndexOrError_ n)
  | n >= (#const paNoError) = fromIntegral n
  | otherwise               = throwAsError n

toMaybeDeviceIndex :: MaybeDeviceIndex_ -> Maybe DeviceIndex
toMaybeDeviceIndex (MaybeDeviceIndex_ n)
  | n == (#const paNoDevice) = Nothing
  | otherwise                = Just (fromIntegral n)


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Device information


-- | Retrieves a structure containing information about the specified device.
--
-- Returns 'Nothing' if an error occurs.

getDeviceInfo :: DeviceIndex -> IO (Maybe DeviceInfo)
getDeviceInfo ix = getDeviceInfo_ ix_ >>= maybePeek'
  where ix_ = fromDeviceIndex ix

foreign import ccall "portaudio.h Pa_GetDeviceInfo"
  getDeviceInfo_ :: DeviceIndex_ -> IO (Ptr DeviceInfo)


-- | TODO
data DeviceInfo =
  DeviceInfo {
    diName                     :: !String,
    -- ^ TODO
    diHostApiIndex             :: !HostApiIndex,
    -- ^ TODO
    diMaxInputChannelCount     :: !ChannelCount,
    -- ^ TODO
    diMaxOutputChannelCount    :: !ChannelCount,
    -- ^ TODO
    diDefaultLowInputLatency   :: !Double,
    -- ^ TODO
    diDefaultLowOutputLatency  :: !Double,
    -- ^ TODO
    diDefaultHighInputLatency  :: !Double,
    -- ^ TODO
    diDefaultHighOutputLatency :: !Double,
    -- ^ TODO
    diDefaultSampleRate        :: !SampleRate
    -- ^ TODO
  } deriving (Eq, Show)

instance Storable DeviceInfo where
  sizeOf _    = (#size PaDeviceInfo)
  alignment _ = alignment nullPtr
  peek ptr    = do
    vsn       <- peekAt ptr (#offset PaDeviceInfo, structVersion) :: IO CInt
    when (vsn /= 2) (error ("unknown device info version: " ++ show vsn))
    nm_       <- peekAt ptr (#offset PaDeviceInfo, name)
    ix_       <- peekAt ptr (#offset PaDeviceInfo, hostApi)
    inChs_    <- peekAt ptr (#offset PaDeviceInfo, maxInputChannels)
    outChs_   <- peekAt ptr (#offset PaDeviceInfo, maxOutputChannels)
    loInLtc_  :: CDouble <- peekAt ptr (#offset PaDeviceInfo, defaultLowInputLatency)
    loOutLtc_ :: CDouble <- peekAt ptr (#offset PaDeviceInfo, defaultLowOutputLatency)
    hiInLtc_  :: CDouble <- peekAt ptr (#offset PaDeviceInfo, defaultHighInputLatency)
    hiOutLtc_ :: CDouble <- peekAt ptr (#offset PaDeviceInfo, defaultHighOutputLatency)
    rt_       <- peekAt ptr (#offset PaDeviceInfo, defaultSampleRate)
    nm        <- peekCString nm_
    return $ DeviceInfo {
      diName                     = nm,
      diHostApiIndex             = toHostApiIndex ix_,
      diMaxInputChannelCount     = toChannelCount inChs_,
      diMaxOutputChannelCount    = toChannelCount outChs_,
      diDefaultLowInputLatency   = realToFrac loInLtc_,
      diDefaultLowOutputLatency  = realToFrac loOutLtc_,
      diDefaultHighInputLatency  = realToFrac hiInLtc_,
      diDefaultHighOutputLatency = realToFrac hiOutLtc_,
      diDefaultSampleRate        = toSampleRate rt_
    }


------------------------------------------------------------------------------
--
-- * Host APIs


-- | Unchanging unique identifiers for each supported host API.
--
-- New identifiers will be allocated when support for a host API reaches
-- \"public alpha\" status; prior to that developers should use
-- 'InDevelopment'.

data HostApi =
    InDevelopment
    -- ^ TODO
  | DirectSound
    -- ^ TODO
  | MME
    -- ^ TODO
  | ASIO
    -- ^ TODO
  | SoundManager
    -- ^ TODO
  | CoreAudio
    -- ^ TODO
  | OSS
    -- ^ TODO
  | ALSA
    -- ^ TODO
  | AL
    -- ^ TODO
  | BeOS
    -- ^ TODO
  | WDMKS
    -- ^ TODO
  | JACK
    -- ^ TODO
  | WASAPI
    -- ^ TODO
  | AudioScienceHPI
    -- ^ TODO
      deriving (Eq, Show)

newtype HostApi_ = HostApi_ CInt
  deriving (Eq, Show, Storable)

instance Enum HostApi where
  fromEnum e = case e of
    InDevelopment              -> (#const paInDevelopment)
    DirectSound                -> (#const paDirectSound)
    MME                        -> (#const paMME)
    ASIO                       -> (#const paASIO)
    SoundManager               -> (#const paSoundManager)
    CoreAudio                  -> (#const paCoreAudio)
    OSS                        -> (#const paOSS)
    ALSA                       -> (#const paALSA)
    AL                         -> (#const paAL)
    BeOS                       -> (#const paBeOS)
    WDMKS                      -> (#const paWDMKS)
    JACK                       -> (#const paJACK)
    WASAPI                     -> (#const paWASAPI)
    AudioScienceHPI            -> (#const paAudioScienceHPI)
  toEnum n = case n of
    (#const paInDevelopment)   -> InDevelopment
    (#const paDirectSound)     -> DirectSound
    (#const paMME)             -> MME
    (#const paASIO)            -> ASIO
    (#const paSoundManager)    -> SoundManager
    (#const paCoreAudio)       -> CoreAudio
    (#const paOSS)             -> OSS
    (#const paALSA)            -> ALSA
    (#const paAL)              -> AL
    (#const paBeOS)            -> BeOS
    (#const paWDMKS)           -> WDMKS
    (#const paJACK)            -> JACK
    (#const paWASAPI)          -> WASAPI
    (#const paAudioScienceHPI) -> AudioScienceHPI
    _ -> error ("unknown host api type: " ++ show n)

fromHostApi :: HostApi -> HostApi_
fromHostApi = HostApi_ . fromIntegral . fromEnum

toHostApi :: HostApi_ -> HostApi
toHostApi (HostApi_ n) = toEnum (fromIntegral n)


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Host API enumeration


-- | Retrieves the number of available host APIs.  Even if a host API is
-- available, it may have no devices available.
--
-- Throws a 'PortAudioException' if an error occurs.

getHostApiCount :: IO HostApiCount
getHostApiCount = liftM' asHostApiCount getHostApiCount_

foreign import ccall "portaudio.h Pa_GetHostApiCount"
  getHostApiCount_ :: IO HostApiCountOrError_


-- | Retrieves the index of the default host API.  The default host API will
-- be the lowest common denominator host API on the current platform, and is
-- unlikely to provide the best performance.
--
-- Throws a 'PortAudioException' if an error occurs.

getDefaultHostApiIndex :: IO HostApiIndex
getDefaultHostApiIndex = liftM' asHostApiIndex getDefaultHostApiIndex_

foreign import ccall "portaudio.h Pa_GetDefaultHostApi"
  getDefaultHostApiIndex_ :: IO HostApiIndexOrError_


-- | Converts a static host API unique identifier into a runtime host API
-- index.
--
-- Throws a 'PortAudioException' if an error occurs:
--
-- * The 'HostApiNotFound' exception indicates that the host API specified is
-- not available.

getHostApiIndex :: HostApi -> IO HostApiIndex
getHostApiIndex api = liftM' asHostApiIndex (getHostApiIndex_ api_)
  where api_ = fromHostApi api

foreign import ccall "portaudio.h Pa_HostApiTypeIdToHostApiIndex"
  getHostApiIndex_ :: HostApi_ -> IO HostApiIndexOrError_


-- | TODO

type HostApiCount = Int

newtype HostApiCountOrError_ = HostApiCountOrError_ CInt
  deriving (Eq, Show)

asHostApiCount :: HostApiCountOrError_ -> HostApiCount
asHostApiCount (HostApiCountOrError_ n)
  | n >= (#const paNoError) = fromIntegral n
  | otherwise               = throwAsError n



-- | Used to enumerate host APIs at runtime.  Values of this type range from
-- @0@ to the result of 'getHostApiCount' minus @1@.

type HostApiIndex = Int

newtype HostApiIndex_ = HostApiIndex_ CInt
  deriving (Eq, Show, Storable)

newtype HostApiIndexOrError_ = HostApiIndexOrError_ CInt
  deriving (Eq, Show)

toHostApiIndex :: HostApiIndex_ -> HostApiIndex
toHostApiIndex (HostApiIndex_ n) = fromIntegral n

asHostApiIndex :: HostApiIndexOrError_ -> HostApiIndex
asHostApiIndex (HostApiIndexOrError_ n)
  | n >= (#const paNoError) = fromIntegral n
  | otherwise               = throwAsError n

fromHostApiIndex :: HostApiIndex -> HostApiIndex_
fromHostApiIndex = HostApiIndex_ . fromIntegral


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Host API information


-- | Retrieves a structure containing information about a specific host API.
--
-- Returns 'Nothing' if an error occurs.

getHostApiInfo :: HostApiIndex -> IO (Maybe HostApiInfo)
getHostApiInfo ix = getHostApiInfo_ ix_ >>= maybePeek'
  where ix_ = fromHostApiIndex ix

foreign import ccall "portaudio.h Pa_GetHostApiInfo"
  getHostApiInfo_ :: HostApiIndex_ -> IO (Ptr HostApiInfo)


-- | TODO

data HostApiInfo =
  HostApiInfo {
    haiHostApi                  :: !HostApi,
    -- ^ TODO
    haiName                     :: !String,
    -- ^ TODO
    haiDeviceCount              :: !HostApiDeviceCount,
    -- ^ TODO
    haiDefaultInputDeviceIndex  :: !(Maybe DeviceIndex),
    -- ^ TODO
    haiDefaultOutputDeviceIndex :: !(Maybe DeviceIndex)
    -- ^ TODO
  } deriving (Eq, Show)

instance Storable HostApiInfo where
  sizeOf _    = (#size PaHostApiInfo)
  alignment _ = alignment nullPtr
  peek ptr    = do
    vsn     <- peekAt ptr (#offset PaHostApiInfo, structVersion) :: IO CInt
    when (vsn /= 1) (error ("unknown host api info version: " ++ show vsn))
    api_    <- peekAt ptr (#offset PaHostApiInfo, type)
    nm_     <- peekAt ptr (#offset PaHostApiInfo, name)
    devCnt_ <- peekAt ptr (#offset PaHostApiInfo, deviceCount)
    inDev_  <- peekAt ptr (#offset PaHostApiInfo, defaultInputDevice)
    outDev_ <- peekAt ptr (#offset PaHostApiInfo, defaultOutputDevice)
    nm      <- peekCString nm_
    return $ HostApiInfo {
      haiHostApi                  = toHostApi api_,
      haiName                     = nm,
      haiDeviceCount              = toHostApiDeviceCount devCnt_,
      haiDefaultInputDeviceIndex  = toMaybeDeviceIndex inDev_,
      haiDefaultOutputDeviceIndex = toMaybeDeviceIndex outDev_
    }


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Host API device enumeration


-- | Converts a host API specific device index into a standard PortAudio
-- device index.  This function may be used in conjunction with the
-- 'haiDeviceCount' field of 'HostApiInfo' to enumerate all devices for the
-- specified host API.
--
-- Throws a 'PortAudioException' if an error occurs:
--
-- * The 'InvalidHostApi' exception indicates that the host API index is out
-- of range;
--
-- * The 'InvalidDeviceIndex' exception indicates that the device index is out
-- of range.

hostApiDeviceIndexToDeviceIndex ::
     HostApiIndex
     -- ^ TODO
  -> HostApiDeviceIndex
     -- ^ TODO
  -> IO DeviceIndex
hostApiDeviceIndexToDeviceIndex apiIx devIx =
  liftM' asDeviceIndex (hostApiDeviceIndexToDeviceIndex_ apiIx_ devIx_)
    where apiIx_ = fromHostApiIndex apiIx
          devIx_ = fromHostApiDeviceIndex devIx

foreign import ccall "portaudio.h Pa_HostApiDeviceIndexToDeviceIndex"
  hostApiDeviceIndexToDeviceIndex_ ::
       HostApiIndex_
    -> HostApiDeviceIndex_
    -> IO DeviceIndexOrError_


-- | TODO

type HostApiDeviceCount = Int

newtype HostApiDeviceCount_ = HostApiDeviceCount_ CInt
  deriving (Eq, Show, Storable)

toHostApiDeviceCount :: HostApiDeviceCount_ -> HostApiDeviceCount
toHostApiDeviceCount (HostApiDeviceCount_ n) = fromIntegral n


-- | TODO

type HostApiDeviceIndex = Int

newtype HostApiDeviceIndex_ = HostApiDeviceIndex_ CInt
  deriving (Eq, Show)

fromHostApiDeviceIndex :: HostApiDeviceIndex -> HostApiDeviceIndex_
fromHostApiDeviceIndex = HostApiDeviceIndex_ . fromIntegral


--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--
-- ** Host error information


-- | Returns information about the last host error encountered.  This function
-- is provided as a last resort, primarily to enhance debugging by providing
-- clients with access to all available error information.
--
-- This function may fail, return invalid data, or return 'Nothing', unless a
-- PortAudio function has previously thrown the 'UnanticipatedHostError'
-- exception.

getLastHostErrorInfo :: IO (Maybe HostErrorInfo)
getLastHostErrorInfo = getLastHostErrorInfo_ >>= maybePeek'

foreign import ccall "portaudio.h Pa_GetLastHostErrorInfo"
  getLastHostErrorInfo_ :: IO (Ptr HostErrorInfo)


-- | TODO

data HostErrorInfo =
  HostErrorInfo {
    heiHostApi   :: !HostApi,
    -- ^ TODO
    heiErrorCode :: !HostErrorCode,
    -- ^ TODO
    heiErrorText :: !String
    -- ^ TODO
  } deriving (Eq, Show)

instance Storable HostErrorInfo where
  sizeOf _    = (#size PaHostErrorInfo)
  alignment _ = alignment nullPtr
  peek ptr    = do
    api_ <- peekAt ptr (#offset PaHostErrorInfo, hostApiType)
    cd_  <- peekAt ptr (#offset PaHostErrorInfo, errorCode)
    txt_ <- peekAt ptr (#offset PaHostErrorInfo, errorText)
    txt  <- peekCString txt_
    return $ HostErrorInfo {
      heiHostApi   = toHostApi api_,
      heiErrorCode = toHostErrorCode cd_,
      heiErrorText = txt
    }


-- | TODO

type HostErrorCode = Int

newtype HostErrorCode_ = HostErrorCode_ CLong
  deriving (Eq, Show, Storable)

toHostErrorCode :: HostErrorCode_ -> HostErrorCode
toHostErrorCode (HostErrorCode_ n) = fromIntegral n


------------------------------------------------------------------------------
--
-- * Exceptions


-- | Used to represent error codes returned by PortAudio functions.  The Show 
-- instance produces a human-readable message.

data PortAudioException =
    NotInitialized
  | UnanticipatedHostError
  | InvalidChannelCount
  | InvalidSampleRate
  | InvalidDeviceIndex
  | InvalidFlag
  | SampleFormatNotSupported
  | BadIODeviceCombination
  | InsufficientMemory
  | BufferTooBig
  | BufferTooSmall
  | NullCallback
  | BadStreamPtr
  | TimedOut
  | InternalError
  | DeviceUnavailable
  | IncompatibleHostApiSpecificStreamInfo
  | StreamIsStopped
  | StreamIsNotStopped
  | InputOverflowed
  | OutputUnderflowed
  | HostApiNotFound
  | InvalidHostApi
  | CanNotReadFromACallbackStream
  | CanNotWriteToACallbackStream
  | CanNotReadFromAnOutputOnlyStream
  | CanNotWriteToAnInputOnlyStream
  | IncompatibleStreamHostApi
  | BadBufferPtr
      deriving (Eq, Typeable)

newtype Error_ = Error_ CInt
  deriving (Eq, Show)

instance Enum PortAudioException where
  toEnum n = case n of
    (#const paNotInitialized)           -> NotInitialized
    (#const paUnanticipatedHostError)   -> UnanticipatedHostError
    (#const paInvalidChannelCount)      -> InvalidChannelCount
    (#const paInvalidSampleRate)        -> InvalidSampleRate
    (#const paInvalidDevice)            -> InvalidDeviceIndex
    (#const paInvalidFlag)              -> InvalidFlag
    (#const paSampleFormatNotSupported) -> SampleFormatNotSupported
    (#const paBadIODeviceCombination)   -> BadIODeviceCombination
    (#const paInsufficientMemory)       -> InsufficientMemory
    (#const paBufferTooBig)             -> BufferTooBig
    (#const paBufferTooSmall)           -> BufferTooSmall
    (#const paNullCallback)             -> NullCallback
    (#const paBadStreamPtr)             -> BadStreamPtr
    (#const paTimedOut)                 -> TimedOut
    (#const paInternalError)            -> InternalError
    (#const paDeviceUnavailable)        -> DeviceUnavailable
    (#const paIncompatibleHostApiSpecificStreamInfo) ->
      IncompatibleHostApiSpecificStreamInfo
    (#const paStreamIsStopped)          -> StreamIsStopped
    (#const paStreamIsNotStopped)       -> StreamIsNotStopped
    (#const paInputOverflowed)          -> InputOverflowed
    (#const paOutputUnderflowed)        -> OutputUnderflowed
    (#const paHostApiNotFound)          -> HostApiNotFound
    (#const paInvalidHostApi)           -> InvalidHostApi
    (#const paCanNotReadFromACallbackStream) ->
      CanNotReadFromACallbackStream
    (#const paCanNotWriteToACallbackStream) ->
      CanNotWriteToACallbackStream
    (#const paCanNotReadFromAnOutputOnlyStream) ->
      CanNotReadFromAnOutputOnlyStream
    (#const paCanNotWriteToAnInputOnlyStream) ->
      CanNotWriteToAnInputOnlyStream
    (#const paIncompatibleStreamHostApi) ->
      IncompatibleStreamHostApi
    (#const paBadBufferPtr)             -> BadBufferPtr
    _ -> error ("unknown error: " ++ show n)
  fromEnum e = case e of
    NotInitialized                      -> (#const paNotInitialized)
    UnanticipatedHostError              -> (#const paUnanticipatedHostError)
    InvalidChannelCount                 -> (#const paInvalidChannelCount)
    InvalidSampleRate                   -> (#const paInvalidSampleRate)
    InvalidDeviceIndex                  -> (#const paInvalidDevice)
    InvalidFlag                         -> (#const paInvalidFlag)
    SampleFormatNotSupported            -> (#const paSampleFormatNotSupported)
    BadIODeviceCombination              -> (#const paBadIODeviceCombination)
    InsufficientMemory                  -> (#const paInsufficientMemory)
    BufferTooBig                        -> (#const paBufferTooBig)
    BufferTooSmall                      -> (#const paBufferTooSmall)
    NullCallback                        -> (#const paNullCallback)
    BadStreamPtr                        -> (#const paBadStreamPtr)
    TimedOut                            -> (#const paTimedOut)
    InternalError                       -> (#const paInternalError)
    DeviceUnavailable                   -> (#const paDeviceUnavailable)
    IncompatibleHostApiSpecificStreamInfo ->
      (#const paIncompatibleHostApiSpecificStreamInfo)
    StreamIsStopped                     -> (#const paStreamIsStopped)
    StreamIsNotStopped                  -> (#const paStreamIsNotStopped)
    InputOverflowed                     -> (#const paInputOverflowed)
    OutputUnderflowed                   -> (#const paOutputUnderflowed)
    HostApiNotFound                     -> (#const paHostApiNotFound)
    InvalidHostApi                      -> (#const paInvalidHostApi)
    CanNotReadFromACallbackStream ->
      (#const paCanNotReadFromACallbackStream)
    CanNotWriteToACallbackStream ->
      (#const paCanNotWriteToACallbackStream)
    CanNotReadFromAnOutputOnlyStream ->
      (#const paCanNotReadFromAnOutputOnlyStream)
    CanNotWriteToAnInputOnlyStream ->
      (#const paCanNotWriteToAnInputOnlyStream)
    IncompatibleStreamHostApi ->
      (#const paIncompatibleStreamHostApi)
    BadBufferPtr                        -> (#const paBadBufferPtr)

toError :: Error_ -> PortAudioException
toError (Error_ n) = toEnum (fromIntegral n)

fromError :: PortAudioException -> Error_
fromError = Error_ . fromIntegral . fromEnum

instance Show PortAudioException where
  show = unsafePerformIO . peekCString . getErrorText_ . fromError

instance Exception PortAudioException

foreign import ccall "portaudio.h Pa_GetErrorText"
  getErrorText_ :: Error_ -> CString

throwAsError :: (Integral a) => a -> b
throwAsError = throw . toError . Error_ . fromIntegral


------------------------------------------------------------------------------
--
-- * Internal utilities


foldFlags :: (Enum a, Bits b) => [a] -> b
foldFlags = foldl' (.|.) 0 . map (fromIntegral . fromEnum)

matchFlag :: (Enum a, Bits b) => b -> a -> Bool
matchFlag b f = b .&. f_ == f_
  where f_ = (fromIntegral . fromEnum) f


peekAt :: (Storable b) => Ptr a -> Int -> IO b
peekAt ptr = peek . plusPtr ptr

pokeAt :: (Storable b) => Ptr a -> b -> Int -> IO ()
pokeAt ptr val = flip poke val . plusPtr ptr


maybeWith :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
maybeWith Nothing fun    = fun nullPtr
maybeWith (Just val) fun = with val fun

maybeFreeFunPtr :: FunPtr a -> IO ()
maybeFreeFunPtr ptr
  | ptr == nullFunPtr = return ()
  | otherwise         = freeHaskellFunPtr ptr


return' :: (Monad m) => a -> m a
return' !x = return x


-- | This function is used to ensure that any exceptions thrown during the
-- translation of function return values from C to Haskell will not get
-- obstructed by lazy evaluation.

liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f x = liftM f x >>= return'


-- | This function is used to ensure that any exceptions thrown during the
-- translation of structures from C to Haskell will not get obstructed by lazy
-- evaluation.  For this to work, the record fields must also be strict.

maybePeek' :: (Storable a) => Ptr a -> IO (Maybe a)
maybePeek' ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = liftM Just (peek ptr >>= return')
