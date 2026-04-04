module Droste 
  ( Droste.log
  ) where

import Codec.Picture

import Complex

-- todo:
--   figure out width  discrepancy
--   non-centered Drostes
--   implement exp of image function

log :: Image PixelRGB8 -> Float -> Image PixelRGB8
log img scale = generateImage imgNew 
  (floor width)
  (floor height)
    where 
      imgNew x y = pixelAt img (floor x') (floor y')
        where
          y' = (1-(imaginary p3)-pi) * 
               inHeight
          x' = (real p3) * inWidth
          p3 = (Complex.exp(p2) * sc) + 
               (Complex 0.5 (0.5 - pi))
          sc = (Complex (1/(2*e)) 0)
          e  = Prelude.exp(1)
          p2 = Complex ((s*xn) - s + 1) 
               (-2 * yn)
          s  = Prelude.log(1/scale)
          yn = (fromIntegral y) / inHeight
          xn = (fromIntegral x) / inWidth
      height = inHeight * pi
      width = inWidth
      -- width = inWidth * scale * 4
      inHeight = (fromIntegral (imageHeight img) :: Float)
      inWidth = (fromIntegral (imageWidth img) :: Float)