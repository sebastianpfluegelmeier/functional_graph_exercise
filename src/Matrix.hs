module Matrix 
    ( Matrix 
    , getElem
    , elementwise
    , setElem
    , ncols
    , nrows
    , identity
    , matrix
    , fromLists
    , toLists
    , multStd
    )
where 
import Data.Map.Strict
import Data.Function

type Key = (Int, Int)

type Matrix a = Map Key a

getElem :: Int -> Int -> Matrix a -> a
getElem x y matrix = matrix ! (x, y)

elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c 
elementwise operation matrixA matrixB = 
        let keys = toList matrixA & fmap fst
        in  fmap (\(x, y) -> let 
                                 valA      = getElem x y matrixA
                                 valB      = getElem x y matrixB
                                 valResult = operation valA valB
                                 key       = (x, y)
                             in  (key, valResult)
                 ) 
                keys
            & fromList

setElem :: a -> Key -> Matrix a -> Matrix a
setElem val key matrix = 
        insert key val matrix

ncols :: Matrix a -> Int
ncols matrix = toList matrix & fmap (\((x, _), _) -> x) & maximum

nrows :: Matrix a -> Int
nrows matrix = toList matrix & fmap (\((_, y), _) -> y) & maximum

identity :: Int -> Matrix Int
identity size = matrix size size (\(x, y) -> if x == y then 1 else 0)

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix rows columns fn = fromList [((x, y), fn (x, y)) | x <- [1..rows], y <- [1..columns]]

fromLists :: [[a]] -> Matrix a
fromLists lists = matrix (length lists) ((length $ lists !! 0)) (\(x, y) -> lists !! (x - 1) !! (y - 1))

toLists :: Matrix a -> [[a]]
toLists matrix = 
        let indices = [1..ncols matrix]
        in  indices
            & fmap (\x -> fmap (\y -> getElem x y matrix) indices) 

multStd :: Num a => Matrix a -> Matrix a -> Matrix a
multStd matrixA matrixB = matrix (nrows matrixA)
                                 (ncols matrixB)
                                 (\(x, y) -> 
                                    fmap (\i -> getElem x i matrixA + getElem i y matrixB) 
                                         [1..nrows matrixA] 
                                    & sum
                                 )
