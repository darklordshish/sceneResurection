{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude as P
import Graphics.Image as I
import Graphics.Image.Processing.Filter (applyFilter
                                        ,gaussianBlur
                                        ,logFilter)
import System.Environment (getArgs)

import KAZZY (Conductivity(..)
             ,nonlinearDiffusion
             )


convertImg :: Image VS RGB Word8 -> Image VS RGB Double
convertImg img = convert img 

fileName :: String
fileName =  "img/Lenna.jpg"

main :: IO ()
main = do 
  imgRGB <- readImageRGB VS fileName
  --let image = applyFilter sndFilter $ applyFilter fstFilter imgRGB
  let image = nonlinearDiffusion SelectiveSmoothing 10.7 1.6 3 imgRGB !! 1
  writeImageExact PNG [] "LennaDiff.png" (image :: Image VS RGB Double)
  --where fstFilter = gaussianBlur 10
  --      sndFilter = logFilter Edge

-- main :: IO ()
-- main = do
--     commandArguments <- getArgs
--     case commandArguments of
--       [] -> putStrLn "Not enough arguments"
--       (filename : _) -> do
--         imgRGB <- readImageRGB VS filename
--         let convRGB = applyFilter (gaussianBlur 10) imgRGB
--         writeImage "image.png" convRGB

