module Main (main) where

import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types
import Data.Map
import Data.IntMap

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


            -- let imgs = applyN img [(zoom 0.05), (rotate (pi/10))] (1/16) 20
            

            let imgs = applyN img [(zoom 0.05)] (1/16) 20
            let frames = gifZip imgs greyPalette 10

            let gifOut = writeGifImages "piZoom.gif" LoopingForever frames
            case gifOut of
                Left err2 -> putStrLn err2
                Right d -> do d


            -- putStrLn "e"