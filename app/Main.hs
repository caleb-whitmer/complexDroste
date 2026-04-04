module Main (main) where

import Codec.Picture

-- import Complex
import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html



main :: IO ()
main = do
    imRead <- readPng "canvas1.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc
            let imgOut = Droste.log img (1/16)
            writePng "test2.png" imgOut
            -- putStrLn (show (pixelAt img 1 1))
            -- putStrLn (show (imageWidth img))