module Hex where

data Hex = Hex Int Int
data HexStep = Iplus | Iminus | Jplus | Jminus | Kplus | Kminus

xVal (Hex x y) = x

yVal (Hex x y) = y

zVal (Hex x y) = - (x + y)

-- rotate:  x <- -z,
--         -z <-  y, 
--          y <- -x,
--         -x <-  z, 
--          z <- -y, 
--         -y <-  x
            
rotate (Hex x y) = Hex (x + y) (- x)

unrotate (Hex x y) = Hex (- y) (x + y)

-- rotate = (  1 -1 )   unrotate = (  0  1 )
--          (  1  0 )              ( -1  1 )