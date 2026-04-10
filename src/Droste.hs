module Droste 
  ( Droste.scale,
    Droste.overlay,
    Droste.forwardRecurse,
    Droste.pointExp,
    Droste.pointLog,
    Droste.drosteEscher,
  ) where

import Codec.Picture
import Data.Fixed

import Complex

-- !
-- ! @brief      Place a downscaled version of the image in the center of the 
-- !             image. This prevents loss of quality when converting to and
-- !              from  logarithmic space.
-- !
-- ! @return     An version of the image with a high-quality center.
-- !
forwardRecurse :: Image PixelRGB8 -> Float -> Image PixelRGB8
forwardRecurse img s = overlay
  img
  scaledImg
  (floor x')
  (floor y')
    where
      scaledImg = scale img s
      y' = 0.5 * height -               -- 
           0.5 * sHeight                -- 
      x' = 0.5 * width -                -- Get offset to center the original 
           0.5 * sWidth                 -- image inside the outer image.
      sHeight = fromIntegral (imageHeight scaledImg) :: Float
      sWidth  = fromIntegral (imageWidth scaledImg) :: Float
      height  = fromIntegral (imageHeight img) :: Float
      width   = fromIntegral (imageWidth img) :: Float

-- !
-- ! @brief      Overlay on image on top of another at some offset.
-- !
-- ! @return     A combination of the two image, with the second overlayed on 
-- !             top of the first.
-- !
overlay :: Image PixelRGB8 -> Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
overlay bImg tImg oX oY = generateImage imgNew
  (imageWidth bImg)
  (imageHeight bImg)
    where
      imgNew x y = pixelAt cImg x' y'
        where
          cImg = if inTop               --
                 then tImg              -- 
                 else bImg              -- 
          y'   = if inTop               -- 
                 then y - oY            -- 
                 else y                 -- 
          x'   = if inTop               -- Change the image and coordinate
                 then x - oX            -- system if we are "inside" the  
                 else x                 -- overlayed image.
          inTop= (y >= oY && y < (oY + (imageHeight tImg))) &&
                 (x >= oX && x < (oX + (imageWidth tImg)))

-- !
-- ! @brief      Scale an image by some factor.
-- !
-- ! @return     The scaled image.
-- !
scale :: Image PixelRGB8 -> Float -> Image PixelRGB8
scale img s = generateImage imgNew
  (floor width)
  (floor height)
    where 
      imgNew x y = pixelAt img
        (floor x')
        (floor y')
          where
            y' = (1/s)*(fromIntegral y)
            x' = (1/s)*(fromIntegral x)
      height   = s * (fromIntegral (imageHeight img) :: Float)
      width    = s * (fromIntegral (imageWidth img) :: Float)

-- -- !
-- -- ! @brief      Rotate and scale a given input point around a pivot. To convert 
-- -- !             the space to Escher space.
-- -- !
-- -- ! @return     The input point after a rotation and scale has been applied.
-- -- !
pointEscher :: (Float, Float) -> (Float, Float) -> (Float, Float)
pointEscher point pivot = output
  where
    output = ( mod' (real p1) 1,        -- 
               mod' (imaginary p1) pi ) -- Clamp the output to repeat.
                                        -- 
    p1 = pc + ( (Complex s3 0) *        -- 
         d1 * ( Complex.exp             -- Calculate the final point by rotating
         (Complex 0 a2) ) )             -- and scaling around the pivot.
                                        -- 
    s3 = (real (abs (Complex 1 pi)))/pi -- Calculate the scaling factor to keep
                                        -- the length to the adjacent point the
                                        -- same after the rotation takes place.
                                        -- 
    a2 = a1 + ((pi/2) - ac)             -- Calculate the new angle to rotate the
                                        -- space by.
                                        -- 
    a1  = if (l2 >= 0)                  -- 
         then l2                        -- Offset the angle so that it is 
         else l2 + (2 * pi)             -- between 0 and pi
                                        -- 
    l2  = imaginary ( Complex.log       -- Get the angle between the pivot and 
          (input - pc) )                -- the input point.
                                        -- 
    d1  = abs (pc - input)              -- 
    ac = atan2 pi 1.0                   -- Get the angle to the same point in an
                                        -- adjacent repeated log-space image.
                                        -- 
    pc = fromPoint pivot                -- Get normalize imaginary part of pivot.
                                        -- between 0 and pi.
    input = fromPoint point

-- !
-- ! @brief      Convert a point to exponential space.
-- !
-- ! @return     The converted point
-- !
pointExp :: (Float, Float) -> Float -> (Float, Float)
pointExp (x, y) s = fromComplex p3
  where
    p3 = Complex                        -- 
         (mod' (real p2) 1)             -- 
         (mod' (1-(imaginary p2)-pi) 1) -- Convert the space of the final point.
                                        -- 
    p2 = (Complex.exp(p1) * sc) +       -- Final point; placed at some radius
         ( Complex 0.5 (0.5 - pi) )     -- and angle from middle of input image.
                                        -- 
    sc = Complex (1 / (2*e)) 0          -- Scale p2 by this constant.
                                        -- 
    e  = Prelude.exp 1                  -- Euler's number.
                                        -- 
    p1 = ( Complex (s1*x) (-2*y) ) +    -- 
         ( Complex (-s1+1) 0 )          -- Intermediate point.
                                        -- 
    s1  = Prelude.log (1 / s)           -- Factor to scale x component of p1.
                                        --
-- !
-- ! @brief      Convert a point to logarithmic space.
-- !
-- ! @return     The converted point.
-- !
pointLog :: (Float, Float) -> Float -> (Float, Float)
pointLog (x, y) s = fromComplex p4
  where
    p4 = Complex (real p3)              -- 
         (pi - (abs (imaginary p3) ) )  -- Reverse the y axis.
                                        -- 
    p3 = (c (l-o) (1/s1)) +             -- Convert input point of generateImage
         (Complex 1 (-2*pi))            -- from Cartesian space to log.
      where                             -- 
        c z s2 = Complex ( s2*(real z) )-- Scale the real component of some
                         ( imaginary z )-- imaginary number by s2.
                                        -- 
    l  = Complex.log p2                 -- 
                                        -- Natural log of p2 for building p3.
    o  = Complex.log (Complex 0.5 0)    -- Offset constant of x component of p3.
                                        -- 
    s1  = Prelude.log (1 / s)           -- Scale factor for x component of p3.
                                        -- 
    p2 = (abs p1)*( Complex.exp         -- 
         (Complex 0 (0.5*a)) )          -- Intermediate point,
                                        -- 
    a  = atan2 (y-0.5) (x-0.5)          -- Get p1's angle from the origin.
                                        -- 
    p1 = (Complex x y)-(Complex 0.5 0.5)-- p1 is x,y centered around the origin.


drosteEscher :: Image PixelRGB8 -> (Float, Float) -> Float -> Image PixelRGB8
drosteEscher img pivot s = generateImage imgNew width height
  where
    imgNew x y = pixelAt img x' y'
      where
        y' = mod (floor (v * h)) height -- De-normalize the point and clamp with
        x' = mod (floor (u * w)) width  -- repeating.
                                        -- 
        (u, v)                          -- Convert the point back to Cartesian
           = pointExp p2 s              -- space.
                                        -- 
        p2 = pointEscher p1 pivot'      -- Convert to Escher space by rotating
                                        -- and scaling the point around a given 
                                        -- pivot.
                                        -- 
        p1 = pointLog (xn, yn) s        -- Convert the point to Logarithmic 
                                        -- space.
                                        -- 
        yn = (fromIntegral y) / h       -- 
        xn = (fromIntegral x) / w       -- Normalize the point between 0 and 1.
                                        -- 
        h  = ( fromIntegral height ::   -- 
               Float )                  -- 
        w  = ( fromIntegral width ::    -- 
               Float )                  -- Get the dimensions as floats.
                                        -- 
    pivot' = (pointLog pivot s)         -- Convert the pivot to logarithmic
                                        -- space.
                                        -- 
    height = (imageHeight img)          -- 
    width  = (imageWidth img)           -- Get the dimensions of the image.