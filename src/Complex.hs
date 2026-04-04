module Complex 
  ( Complex (..),
    real,
    imaginary,
    Complex.exp,
    Complex.log
  ) where 

data Complex r i = Complex Float Float
  deriving (Eq)

real :: (Complex w w) -> Float          -- 
real (Complex r _) = r                  -- Get the real part.

imaginary :: (Complex w w) -> Float     --
imaginary (Complex _ i) = i             -- Get the imaginary part. 

instance Num (Complex r i) where        -- Implement Complex as a Num instance
  (Complex x y) + (Complex u v) =       -- 
    Complex (x + u) (y + v)             -- Addition
  (Complex x y) - (Complex u v) =       -- 
    Complex (x - u) (y - v)             -- Subtraction
  (Complex a b) * (Complex c d) =       -- 
    Complex ((a*c)-(b*d)) ((a*d)+(b*c)) -- Multiplication
  abs (Complex r i) =                   -- 
    Complex (sqrt (r**2 + i**2)) 0      -- Absolute Value
  signum (Complex r i) =                -- 
    Complex (r/m) (i/m)                 -- 
      where m = sqrt (r**2 + i**2)      -- Sign Function
  fromInteger a =                       -- 
    Complex (fromIntegral a :: Float) 0 -- From Integer

instance Show (Complex r i) where       -- Show override:
  show (Complex r i) = if r == 0        -- If the real part is zero:
    then (show i) ++ "i"                -- Then only show the imaginary part. 
    else                                -- Otherwise:
      case (signum i) of                -- When the imaginary part is:
        0  -> (show r)                  -- zero, show only the real part;
        -1 -> (show r) ++ " - " ++      -- negative one, show the real part 
              (show (abs i)) ++ "i"     -- minus the abs of imaginary part;
        _  -> (show r) ++ " + " ++      -- anything else, show the real part
              (show i) ++ "i"           -- plus the imaginary part.

exp :: (Complex w w) -> (Complex w w)   -- Exponential of Complex Numbers:
exp (Complex r i) =                     -- exp(x + yi) =
  (Complex (Prelude.exp r) 0) *         --  exp(x) *
  (Complex (cos i) (sin i))             --  ((cos x) + i(sin y))

log :: (Complex w w) -> (Complex w w)   -- Natural Log of Complex Numbers:
log (Complex r i) =                     -- ln(x +  yi) =
  Complex (Prelude.log m) (atan2 i r)   --  ln(sqrt(x**2 + y**2)) +
    where m = sqrt (r**2 + i**2)        --  atan2(y, x)i