{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Array (pokeArray)
import Sound.PortAudio (SampleFormatFlag (..), StreamCallbackResult (..),
                        withDefaultStream)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["saw", f, t] -> play (saw (read f)) (read t)
    ["sine", f, t] -> play (sine (read f)) (read t)
    ["square", f, t] -> play (square (read f)) (read t)
    _ -> putUsage

putUsage :: IO ()
putUsage = putStr $ unlines $
  ["Usage:",
   "  play saw|sine|square <f> <s>  play a simple sound wave with frequency",
   "                                <f> for <s> seconds"]


type Time = Double
type Amplitude = Double
type Frequency = Double
type Generator = Time -> Amplitude

play :: Generator -> Time -> IO ()
play g t = do
  fmRef <- newIORef 0
  let cb = \ _ outPtr fmCt _ _ -> do
        fm <- readIORef fmRef
        let fms = [fm .. fm + fmCt - 1]
            ts = map ((/ 44100) . realToFrac) fms
            spls :: [CFloat] = map (realToFrac . g) ts
        writeIORef fmRef (fm + fmCt)
        pokeArray outPtr spls
        return Continue
  withDefaultStream 0 1 [Float32] 44100 4096 (Just cb) (const (sleep t))

sine :: Frequency -> Generator
sine f t = sin (2 * pi * f * t)

saw :: Frequency -> Generator
saw f t = 2 * fraction (f * t + 0.5) - 1

square :: Frequency -> Generator
square f t =
  if fraction (f * t) < 0.5
    then 1
    else -1

fraction :: Double -> Double
fraction a = b
  where (_ :: Int, b) = properFraction a

sleep :: Time -> IO ()
sleep = threadDelay . round . (1000000 *)
