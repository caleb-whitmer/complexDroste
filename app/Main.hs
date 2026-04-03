module Main (main) where

import Codec.Picture

pR :: Int -> Int -> PixelRGB8           -- 
pR x y = PixelRGB8                      -- 
    (floor (((fromIntegral x :: Float) / 400)*255))-- 
    (floor (((fromIntegral y :: Float) / 400)*255))-- 
    0 

main :: IO ()
main = writePng "test.png" $ generateImage pR 400 400
