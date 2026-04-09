module Main (main) where

import Codec.Picture

import Complex
import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html


main :: IO ()
main = do
    imRead <- readPng "pilog.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc
            let imgOut = Droste.exp img (1/16) (Just (escher (Complex (1007/1125) (2769/7539))))
            -- let imgOut = Droste.exp img (1/16) Nothing
            writePng "piexp.png" imgOut