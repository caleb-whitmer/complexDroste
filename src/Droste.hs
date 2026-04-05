module Droste 
  ( Droste.log
  ) where

import Codec.Picture

import Complex

-- todo:
--   non-centered Droste
--   implement exp of image function

log :: Image PixelRGB8 -> Float -> Image PixelRGB8
log img scale = generateImage imgNew 
  (floor width)
  (floor height)
    where 
      imgNew x y = pixelAt img 
        (floor x')
        (floor y')
          where
            y' = (1-(imaginary p3)-pi)* -- Get y component (imaginary) placing
                 inHeight               -- it in first quadrant and scaling.
                                        -- 
            x' = (real p3) * inWidth    -- Get x component (real & 1st quad).
                                        -- 
            p3 = (Complex.exp(p2)*sc) + -- Final point; placed at some radius
                 (Complex 0.5 (0.5-pi)) -- and angle from middle of input image.
                                        -- 
            sc = (Complex (1/(2*e)) 0)  -- Scale p3 by this constant.
                                        -- 
            e  = Prelude.exp(1)         -- Euler's number.
                                        -- 
            p2 = Complex ((s*xn)-s+1)   -- 
                 (-2 * yn)              -- Intermediate point.
                                        -- 
            s  = Prelude.log(1 / scale) -- Factor to scale x component of p2.
                                        -- 
            yn = (fromIntegral y) *     -- 
                 (pi/height)            -- Normalize y between 0 and pi.
                                        -- 
            xn = (fromIntegral x) *     -- 
                 (1/width)              -- Normalize x between 0 and 1.
                                        -- 
      height = inHeight * pi            -- Height is max circumference of input.
      width = inWidth * (1-scale) * 0.5 -- Width is one minus scale all over 2.
      inHeight = (fromIntegral (imageHeight img) :: Float)
      inWidth = (fromIntegral (imageWidth img) :: Float)