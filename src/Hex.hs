module Hex where

data Axis = I | J | K deriving Show

data HexStep = HexStep Axis Bool deriving Show


data Hex = Hex Int Int

data PathSet = PathSet Int Int Int deriving Show

data PathLR = Zero | PathLR HexStep Int Int deriving Show -- First int > 0 , second int >= 0



xVal (Hex x y) = x
yVal (Hex x y) = y
zVal (Hex x y) = - (x + y)

instance Show Hex where
    show h = "Hex " ++ show (xVal h) ++ " " ++ show (yVal h) ++ " " ++ show (zVal h)

toSign True  n =  n
toSign False n = -n

pathLRFromStep hs = PathLR hs 1 0

pathFromStep (HexStep I b) n = PathSet   (toSign b n) 0 0
pathFromStep (HexStep J b) n = PathSet   0 (toSign b n) 0
pathFromStep (HexStep K b) n = PathSet   0 0 (toSign b n)

rightPath Zero           = PathSet 0 0 0
rightPath (PathLR h a b) = pathFromStep h a

leftPath Zero           = PathSet 0 0 0
leftPath (PathLR h a b) = pathFromStep (rotateHS h) b

pathFromPathLR plr = addPaths (rightPath plr) (leftPath plr)

addPaths (PathSet i1 j1 k1) (PathSet i2 j2 k2) = PathSet (i1 + i2) (j1 + j2) (k1 + k2)


-- ( x )               ( i )   (  0 -1  1 ) ( i )
-- ( y ) = hexFromPath ( j ) = (  1  0 -1 ) ( j )
-- ( z )               ( k )   ( -1  1  0 ) ( k )

hexFromPath (PathSet i j k) = Hex (k - j) (i - k)



pathFromHex I h = PathSet         0    (zVal h) (- yVal h)
pathFromHex J h = PathSet (- zVal h)         0    (xVal h)
pathFromHex K h = PathSet   (yVal h) (- xVal h)         0

pathLRFromHex h 
    | x == 0   && y == 0      = Zero
    | yy > xx  && yy >= zz    = PathLR (HexStep I (y > 0)) zz xx
    | zz > yy  && zz >= xx    = PathLR (HexStep J (z > 0)) xx yy
    | xx > zz  && xx >= yy    = PathLR (HexStep K (x > 0)) yy zz
    | otherwise               = undefined
        where x  = xVal h
              y  = yVal h
              z  = zVal h
              xx = abs x
              yy = abs y
              zz = abs z
              
{---------|---------|---------|---------|---------|---------|---------|---------}


-- rotate:  x -> -z,
--         -z ->  y, 
--          y -> -x,
--         -x ->  z, 
--          z -> -y, 
--         -y ->  x

--   rotate ( x ) = (  0 -1 ) ( x )  
--          ( y )   (  1  1 ) ( y )  

-- unrotate ( x ) = (  1  1 ) ( x )
--          ( y )   ( -1  0 ) ( y )

rotate (Hex x y) = Hex (- y) (x + y)
            
unrotate (Hex x y) = Hex (x + y) (- x)

rotateHS (HexStep I b) = HexStep K (not b)
rotateHS (HexStep J b) = HexStep I (not b)
rotateHS (HexStep K b) = HexStep J (not b)

unrotateHS (HexStep I b) = HexStep J (not b)
unrotateHS (HexStep J b) = HexStep K (not b)
unrotateHS (HexStep K b) = HexStep I (not b)


rotatePS   (PathSet i j k) = PathSet (-j) (-k) (-i)
unrotatePS (PathSet i j k) = PathSet (-k) (-i) (-j)

rotatePLR Zero = Zero
rotatePLR (PathLR hs a b) = PathLR (rotateHS hs) a b

unrotatePLR Zero = Zero
unrotatePLR (PathLR hs a b) = PathLR (unrotateHS hs) a b

{---------|---------|---------|---------|---------|---------|---------|---------}

oneNorm h = (abs (xVal h) + abs (yVal h) + abs (zVal h)) `div` 2

twoNorm (Hex x y) = x * x + x * y + y * y

pathLength (PathSet i j k) = abs i + abs j + abs k

pathLRLength (PathLR _ a b) = a + b



