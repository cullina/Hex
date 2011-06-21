module HexRegion where

import Hex

data Region = Region {
      xPlus  :: Int
    , xMinus :: Int
    , yPlus  :: Int
    , yMinus :: Int
    , zPlus  :: Int
    , zMinus :: Int
    }

singleHexRegion hex =
    Region x x y y z z
        where x = xVal hex
              y = yVal hex
              z = zVal hex

    
isSubsetOf a b = 
      xPlus  a <= xPlus  b &&
      yPlus  a <= yPlus  b &&
      zPlus  a <= zPlus  b &&
      xMinus a >= xMinus b &&
      yMinus a >= yMinus b &&
      zMinus a >= zMinus b
      

canonize (Region xPlus xMinus yPlus yMinus zPlus zMinus) =
    let xPlus'  = min xPlus (-yMinus - zMinus)
        yPlus'  = min yPlus (-xMinus - zMinus)
        zPlus'  = min zPlus (-xMinus - yMinus)
        xMinus' = max xMinus (-yPlus - zPlus)
        yMinus' = max yMinus (-xPlus - zPlus)
        zMinus' = max zMinus (-xPlus - yPlus)
    in Region xPlus' xMinus' yPlus' yMinus' zPlus' zMinus'

perimeter (Region xPlus xMinus yPlus yMinus zPlus zMinus) =
    (
    -(xPlus + yMinus + zMinus),
    zMinus + xPlus + yPlus,
    -(yPlus + zMinus + xMinus),
    xMinus + yPlus + zPlus,
    -(zPlus + xMinus + yMinus),
    yMinus + zPlus + xPlus
    )


    
