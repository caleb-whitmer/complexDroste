module Droste 
  ( Droste.log,
    Droste.exp
  ) where

import Codec.Picture
import Data.Fixed

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
            y' = (1-(imaginary p2)-pi)* -- Get y component (imaginary) placing
                 inHeight               -- it in first quadrant and scaling.
                                        -- 
            x' = (real p2) * inWidth    -- Get x component (real & 1st quad).
                                        -- 
            p2 = (Complex.exp(p1)*sc) + -- Final point; placed at some radius
                 (Complex 0.5 (0.5-pi)) -- and angle from middle of input image.
                                        -- 
            sc = (Complex (1/(2*e)) 0)  -- Scale p2 by this constant.
                                        -- 
            e  = Prelude.exp 1          -- Euler's number.
                                        -- 
            p1 = Complex ((s*xn)-s+1)   -- 
                 (-2 * yn)              -- Intermediate point.
                                        -- 
            s  = Prelude.log (1 / scale)-- Factor to scale x component of p1.
                                        -- 
            yn = (fromIntegral y) *     -- 
                 (pi/height)            -- Normalize y between 0 and pi.
                                        -- 
            xn = (fromIntegral x) *     -- 
                 (1/width)              -- Normalize x between 0 and 1.
                                        -- 
      height   = inHeight * pi          -- Height is max circumference of input.
      width    = inWidth*(1 - scale)*0.5-- Width is one minus scale all over 2.
      inHeight = (fromIntegral (imageHeight img) :: Float)
      inWidth  = (fromIntegral (imageWidth img) :: Float)

exp :: Image PixelRGB8 -> Float -> Image PixelRGB8
exp img scale = generateImage imgNew
  (floor width)
  (floor height)
    where
      imgNew x y = pixelAt img
        (floor x')
        (floor y')
          where
            y' = (pi-(abs               -- 
                 (imaginary p3))) *     -- Get y component scaled and in first
                 inHeight * (1/pi)      -- quadrant.
                                        -- 
            x' = (real p3) * inWidth    -- Get x component (real & first quad).

            p3 = (d ( (c (l-o) (1/s)) + -- Final point: converts from log space
                 (Complex 1 (-pi)) )) + -- back to Cartesian
                 (Complex 0 (-pi))      -- 
              where
                c z s1 = Complex (s1*(real z)) (imaginary z)
                                        -- Scale x component of some imaginary
                                        -- number by s1.
                d z = Complex  (mod' (real z) 1) (mod' (imaginary z) pi)
                                        -- Take modulus of x and y components
                                        -- to keep them within the bounds of the
                                        -- log image.
                                        -- 
            l  = Complex.log p2         -- Natural log of p2 for building p3.
                                        -- 
            o  = Complex.log            -- 
                 (Complex 0.5 0)        -- Offset constant of x component of p3.
                                        -- 
            s  = Prelude.log (1 / scale)-- Scale factor for x component of p3.
                                        -- 
            p2 = (abs p1)*( Complex.exp -- 
                 (Complex 0 (0.5*a)) )  -- Intermediate point.
                                        -- 
            a  = atan2 (yn-0.5) (xn-0.5)-- Get p1's angle from origin.
                                        -- 
            p1 = (Complex xn yn) -      -- 
                 (Complex 0.5 0.5)      -- p1 is x,y centered around origin.
                                        -- 
            yn = (fromIntegral y) /     -- 
                 height                 -- Normalize y.
                                        -- 
            xn = (fromIntegral x) /     -- 
                 width                  -- Normalize x.
                                        -- 
      height   = inHeight / pi          -- 
      width    = (inWidth*2)/(1-scale)  -- 
      inHeight = (fromIntegral (imageHeight img) :: Float)
      inWidth  = (fromIntegral (imageWidth img) :: Float)