module Main (main) where

import Codec.Picture

import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html


main :: IO ()
main = do
    imRead <- readPng "canvas.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc
            let imgOut = drosteEscher img ((631/2400), (1900/2400)) (1/16)
            -- let imgOut = drosteEscher img ((998/1550), (322/1550)) (1/5)
            writePng "canvasEscher.png" imgOut