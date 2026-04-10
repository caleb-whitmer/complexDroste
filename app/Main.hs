module Main (main) where

import Codec.Picture

import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html


main :: IO ()
main = do
    imRead <- readPng "pi.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc
            let img1 = scale img 4
            let img2 = forwardRecurse img1 (1/16)
            let imgOut = drosteEscher img2 ((631/2400), (1900/2400)) (1/16)
            -- let imgOut = scale img1 (1/8)
            -- let imgOut = drosteEscher img ((998/1550), (322/1550)) (1/5)
            writePng "piEscher.png" imgOut