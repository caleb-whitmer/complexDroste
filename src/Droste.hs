module Droste 
  ( Droste.log,
    Droste.exp,
    Droste.scale,
    Droste.overlay
  ) where

import Codec.Picture
import Data.Fixed

import Complex

overlay :: Image PixelRGB8 -> Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
overlay bImg tImg oX oY = generateImage imgNew
  (imageWidth bImg)
  (imageHeight bImg)
    where
      imgNew x y = pixelAt cImg
        x'
        y'
          where
            cImg = if (inTY&&inTX)
                   then tImg
                   else bImg
            y'   = if (inTY&&inTX)
                   then y - oY
                   else y
            x'   = if (inTY&&inTX)
                   then x - oX
                   else x
            inTY = (y >= oY && y < (oY + (imageHeight tImg)))  
            inTX = (x >= oX && x < (oX + (imageWidth tImg)))

scale :: Image PixelRGB8 -> Float -> Image PixelRGB8
scale img s = generateImage imgNew
  (floor width)
  (floor height)
    where 
      imgNew x y = pixelAt img
        (floor x')
        (floor y')
          where
            y' = (1/s) * 
                 (fromIntegral y)
            x' = (1/s) * 
                 (fromIntegral x)
      height   = s * (fromIntegral (imageHeight img) :: Float)
      width    = s * (fromIntegral (imageWidth img) :: Float)

log :: Image PixelRGB8 -> Float -> Image PixelRGB8
log img s = generateImage imgNew 
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
            p1 = Complex ((s1*xn)-s1+1)   -- 
                 (-2 * yn)              -- Intermediate point.
                                        -- 
            s1  = Prelude.log (1 / s)   -- Factor to scale x component of p1.
                                        -- 
            yn = (fromIntegral y) *     -- 
                 (pi/height)            -- Normalize y between 0 and pi.
                                        -- 
            xn = (fromIntegral x) *     -- 
                 (1/width)              -- Normalize x between 0 and 1.
                                        -- 
      height   = inHeight * pi          -- Height is max circumference of input.
      width    = inWidth*(1 - s)*0.5    -- Width is one minus scale all over 2.
      inHeight = (fromIntegral (imageHeight img) :: Float)
      inWidth  = (fromIntegral (imageWidth img) :: Float)

exp :: Image PixelRGB8 -> Float -> Image PixelRGB8
exp img s = generateImage imgNew
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

            p3 = (d ( (c (l-o) (1/s1))+ -- Final point: converts from log space
                 (Complex 1 (-pi)) )) + -- back to Cartesian
                 (Complex 0 (-pi))      -- 
              where
                c z s2 = Complex (s2*(real z)) (imaginary z)
                                        -- Scale x component of some imaginary
                                        -- number by s2.
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
            s1  = Prelude.log (1 / s)   -- Scale factor for x component of p3.
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
      width    = (inWidth*2)/(1-s)      -- 
      inHeight = (fromIntegral (imageHeight img) :: Float)
      inWidth  = (fromIntegral (imageWidth img) :: Float)