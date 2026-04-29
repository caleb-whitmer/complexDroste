module Main (main) where

import Codec.Picture
import Droste

writeGif :: String -> [Image PixelRGB8] -> Int -> IO()
writeGif path frames delay = do
  let out = writeGifImages path LoopingForever (createGif frames delay)
  case out of
    Left  err -> putStrLn err
    Right wrt -> do wrt
  where
    createGif imgs d = case imgs of
      (i:is) -> [(p, d, gi)] ++ 
                (createGif is d)
        where
          (gi, p) = palettize opt i
      []     -> []
      where
        opt = PaletteOptions 
              (MedianMeanCut) 
              True 
              256

main :: IO ()
main = do
  imRead <- readPng "io/pi.png"
  case imRead of
    Left err -> putStrLn err
    Right imSuc -> do
      let img = convertRGB8 imSuc
      let frames = applyN img [escher (161/600, 453/600)] [zoom (-1/40)] (1/16) 40
      writeGif "io/escher.gif" frames 6