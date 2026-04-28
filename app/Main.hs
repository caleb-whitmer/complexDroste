module Main (main) where

import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types
import Data.Map
import Data.IntMap

import Droste

-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture-Types.html


gifZip :: [Image PixelRGB8] -> GifDelay -> [(Palette, GifDelay, Image Pixel8)]
gifZip imgs d = case imgs of
    (i:is) -> [(p, d, gi)] ++ (gifZip is d)
        where
            (gi, p) = palettize opt i
    []     -> []
    where
        opt = PaletteOptions (MedianMeanCut) True 256



main :: IO ()
main = do
    imRead <- readPng "pi.png"
    case imRead of
        Left err -> putStrLn err
        Right imSuc -> do
            let img = convertRGB8 imSuc
            -- let imgOut = apply img [scale 0.5] (1/16) 
            -- writePng "piOut.png" imgOut
            

            let imgs = applyN img [(scale (0.95, 1))] (1/16) 20
            let frames = gifZip imgs 10

            let gifOut = writeGifImages "piZoom.gif" LoopingForever frames
            case gifOut of
                Left err2 -> putStrLn err2
                Right d -> do d


            -- putStrLn "e"