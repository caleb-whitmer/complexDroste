module Main (main) where

import Codec.Picture

-- import Complex
import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html



main :: IO ()
main = do
    imRead <- readPng "droste.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc
            let imgScale = Droste.scale img (1/2)
            let imgOut = Droste.overlay img imgScale 100 100
            writePng "drosteoverlay.png" imgOut