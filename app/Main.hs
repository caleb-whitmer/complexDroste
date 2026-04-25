module Main (main) where

import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types

import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html

red (PixelRGB8 r _ _) = r

imgRed img = generateImage imgNew (imageWidth img) (imageHeight img)
    where
        imgNew x y = red (pixelAt img x y)

gifZip :: [Image PixelRGB8] -> Palette -> GifDelay -> [(Palette, GifDelay, Image Pixel8)]
gifZip imgs p d = case imgs of
    (i:is) -> [(p, d, imgRed i)] ++ (gifZip is p d)
    []     -> []


main :: IO ()
main = do
    imRead <- readPng "pi.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc

            -- let imgOut = apply img [(escher ((631/2400), (1900/2400)))] (1/16)
            -- let imgOut = apply img [(zoom 0.05), (zoom 0.05), (zoom 0.05), (zoom 0.05), (zoom 0.05), (zoom 0.05), (zoom 0.05), (zoom 0.05), (zoom 0.05)] (1/16)
            -- let imgOut = apply img [(zoom 0.45)] (1/16)
            -- let img1 = scale img 4
            -- let img2 = forwardRecurse img1 (1/16)
            -- let imgOut = drosteEscher img2 ((631/2400), (1900/2400)) (1/16)
            -- writePng "piEscher.png" imgOut


            -- let l = getLogs 600 600 (1/16)
            -- let e = getExps l 600 600 (1/16)
            -- -- putStrLn (show (length (e !! 599)))
            -- putStrLn (show ((e !! 599) !! 599))

            -- let imgs = applyN img [(zoom 0.05)] (1/16) 20
            let imgs = applyN img [(zoom 0.05), (rotate (pi/10))] (1/16) 20
            let frames = gifZip imgs greyPalette 10

            let gifOut = writeGifImages "piZoom.gif" LoopingForever frames
            case gifOut of
                Left err2 -> putStrLn err2
                Right d -> do d


            -- putStrLn "e"