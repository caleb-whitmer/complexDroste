module Droste 
  ( Droste.DrosteTransform,
    Droste.imageScale,
    Droste.imageOverlay,
    Droste.imageForwardRecurse,
    Droste.apply,
    Droste.applyN,
    Droste.zoom,
    Droste.rotate,
    Droste.escher,
    Droste.scale,
  ) where

import Codec.Picture
import Data.Fixed
import Data.Map

import Complex

-- ! 
-- ! Custom type, Takes an input point (normalized to (1, pi)) and a scale and 
-- ! returns an output point. 
-- ! 
type DrosteTransform = (Float, Float) -> Float -> (Float, Float)

-- !
-- ! @brief      Place a downscaled version of the image in the center of the 
-- !             image. This prevents loss of quality when converting to and
-- !              from  logarithmic space.
-- !
-- ! @return     An version of the image with a high-quality center.
-- !
imageForwardRecurse :: Image PixelRGB8 -> Float -> Image PixelRGB8
imageForwardRecurse img s = imageOverlay
  img
  scaledImg
  (floor x')
  (floor y')
    where
      scaledImg = imageScale img s
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
imageOverlay :: Image PixelRGB8 -> Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
imageOverlay bImg tImg oX oY = generateImage imgNew
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
imageScale :: Image PixelRGB8 -> Float -> Image PixelRGB8
imageScale img s = generateImage imgNew
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

-- !
-- ! @brief      Convert a point to exponential space.
-- !
-- ! @return     The converted point
-- !
pointExp :: DrosteTransform
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
pointLog :: DrosteTransform
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


-- !
-- ! @brief      Rotate and scale a given input point around a pivot. To convert 
-- !             the space to Escher space.
-- !
-- ! @return     The input point after a rotation and scale has been applied.
-- !
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
                                        -- 
    input = fromPoint point             -- Get the input point as a complex
                                        -- number.

-- !
-- ! @brief      Applies a list of functions to the image after it has been 
-- !             converted to logarithmic space, the image is converted back to 
-- !             Cartesian space before returning.
-- !
-- ! @return     The image after the functions have been applied.
-- !
apply :: Image PixelRGB8 -> [DrosteTransform] -> Float -> Image PixelRGB8
apply img funcs s = generateImage imgNew width height
  where
    imgNew x y = pixelAt img x' y'
      where
        y' = mod (floor(v*h)) height    -- De-normalize the point and clamp with
        x' = mod (floor(u*w)) width     -- repeating.
                                        -- 
        (u, v)                          -- Convert the point back to Cartesian
           = pointExp p2 s              -- space.
                                        -- 
        p2 = apply' funcs p1            -- Apply each function to the point in 
                                        -- logarithmic space.
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
    height = (imageHeight img)          -- 
    width  = (imageWidth img)           --
    apply' fs p = case fs of            -- 
      (g:gs) -> apply' gs (g p s)       -- Apply function to apply each function
      []     -> p                       -- in the list to the given input point.

-- !
-- ! @brief      Gets a map of Cartesian space points to their logarithmic space
-- !             counter parts.
-- !
-- ! @return     The map of logarithmic points.
-- !
getLogMap :: Int -> Int -> Float -> Map (Int, Int) (Float, Float)
getLogMap w h s = fromList (getList 0 0)-- Generate the map from the list.
  where
    getList x y = 
      if (x >= w && y >= h - 1)         -- If we have reached past the last 
      then []                           -- pixel then return the base case                                         
                                        -- (empty list).
      else if (x >= w)                  -- Otherwise if we reached end of row.
           then (getList 0 (y + 1))     -- Then move to next row and restart.
           else [((x, y),               -- 
                 pointLog (xn,yn) s)]++ -- Otherwise insert the current point 
                (getList (x + 1) y)     -- with its logarithm into the list.
        where                           -- 
          yn = (fromIntegral y) /       -- 
               (fromIntegral h)         -- 
          xn = (fromIntegral x) /       -- 
               (fromIntegral w)         -- Normalize x and y.

-- !
-- ! @brief      Apply a series of transforms to the logarithmic space of an 
-- !             image N times.
-- !
-- ! @return     A list of frames, in order by generation of applied transforms. 
-- !
applyN :: Image PixelRGB8 -> [DrosteTransform] -> Float -> Integer -> [Image PixelRGB8]
applyN img funcs s n = applyN' coordMap n
  where
    applyN' c n' =                      -- 
      if (n' <= 0)                      -- 
      then []                           -- 
      else [generateImage newImg w h]++ -- Generate each frame by apply all 
          (applyN' (app funcs c) (n'-1))-- transforms to the points from the 
                                        -- previous frame.
        where                           -- 
          newImg x y = pixelAt img x' y'-- Generate new image pixel-by-pixel.
            where                       -- 
              y' = mod                  -- 
                   ( floor (            -- 
                     v*(fromIntegral h) -- 
                   ) )                  -- 
                   h                    -- 
              x' = mod                  -- 
                   ( floor (            -- 
                     u*(fromIntegral w) -- 
                   ) )                  --
                   w                    --  Denormalize the output points.
                                        -- 
              (u, v) = pointExp p2 s    -- Take the exponential function of the 
                                        -- output of the logarithmic map lookup.
                                        -- 
              p2 = case p1 of           -- 
                Just p -> p             -- 
                _      -> (0, 0)        -- Look up the point in the logarithmic
              p1 = Data.Map.lookup      -- space map, returning (0, 0) if it is
                   (x, y) c             -- not found.
                                        -- 
          app fs' c' = case fs' of      -- 
            (f:fs) -> app               -- 
                      fs                -- 
                      (  Data.Map.map   -- 
                         (\l -> f l s)  -- 
                         c'  )          -- 
                                        -- Apply a set of functions across the
            []     -> c'                -- logarithmic space map.
                                        -- 
    coordMap = getLogMap w h s          -- Get a map of all Cartesian space 
                                        -- points to their logarithmic space
                                        -- counter-parts.
    h = imageHeight img                 -- 
    w = imageWidth img                  -- Get dimensions of image.

-- !
-- ! @brief      'Zoom' a Droste image by moving the point along the x-axis in 
-- !             Logarithmic space.
-- !
-- ! @return     A Droste transform which modifies the x-position of the input
-- !             point to simulate a zoom operation.
-- !
zoom :: Float -> DrosteTransform
zoom fact (x, y) _ = (x', y)
    where
        x' = if (fact' == 0)            -- If our zoom is negligible then do not
             then x                     -- offset x in order to preserve 
                                        -- resolution.
             else mod' (x + fact') 1    -- Otherwise offset x by the factor.
                                        -- 
        fact' = mod' fact 1             -- Clamp the factor to 1 as the image 
                                        -- repeats on zoom.

-- !
-- ! @brief      'Rotate' a Droste image by moving the point along the y-axis in 
-- !             Logarithmic space.
-- !
-- ! @return     A Droste transform which modifies the y-position of the input 
-- !             point to simulate a rotation operation.
-- !
rotate :: Float -> DrosteTransform
rotate ang (x, y) _ = (x, y')
  where
    y' = mod' (y + (ang/2)) pi          -- Divide the angle by two as we are 
                                        -- clamped to pi rather than 2*pi. Then 
                                        -- use it to offset the y-axis and 
                                        -- clamp.

-- !
-- ! @brief      Scale the x and y axes of the logarithmic space by the 
-- !             components of a given vector.
-- !
-- ! @return     A Droste transform which scales the input point both 
-- !             horizontally and vertically around the center.
-- !
scale :: (Float, Float) -> DrosteTransform
scale (fx, fy) (x, y) _ = (x', y')
  where
    y' = mod' (y * fy) pi               -- Scale each axis of the logarithmic
    x' = mod' (x * fx) 1                -- space by the components of the input.

-- !
-- ! @brief      Take a target pivot point as an input and return a 
-- !             DrosteTransform to convert a Droste Image to an Escher style 
-- !             optical illusion.
-- !
-- ! @return     A function to transform points in Logarithmic space to convert 
-- !             back to Escher style image.
-- !
escher :: (Float, Float) -> DrosteTransform
escher pivot (x, y) s = (x', y')
  where
    (x', y') = pointEscher (x, y) pivot'-- Use the transformed pivot to rotate 
                                        -- and scale the input point based on 
                                        -- the value calculated by pointEscher.
                                        -- 
    pivot' = pointLog pivot s           -- Transform the pivot to logarithmic 
                                        -- space.