module Lib
    ( Graph
    , fromFile
    , fromString
    , (***)
    ) where
import Data.List.Split
import Data.Matrix
import Data.Function
import Data.Maybe
import Control.Monad.Trans.Maybe

type Graph = Matrix Int

adjacencyMatrixValid :: Matrix Int -> Bool
adjacencyMatrixValid matrix = isQuadratic && not hasLoop && not isDirected && allOnesOrZeroes
    where
          sizeRange       = [1..nrows matrix]
          allOnesOrZeroes = all (\x -> x == 0 || x == 1) matrix
          isQuadratic     = nrows matrix == ncols matrix
          hasLoop         = any (\x -> getElem x x matrix /= 0) sizeRange
          isDirected      = any (\(x, y) -> getElem x y matrix /= getElem y x matrix)
                            [(x, y) | x <- sizeRange, y <- sizeRange]

fromString :: String -> Maybe (Matrix Int)
fromString string = fmap fromLists $ sequence $ map (\x -> sequence $ map readBinaryInt x) $ map (splitOn ";") $ splitOn "\n" $ filter (/=' ') string

-- read binary Int from String
readBinaryInt :: String -> Maybe Int
readBinaryInt string
    | string == "1" = Just 1
    | string == "0" = Just 0
    | otherwise     = Nothing

-- read Graph from File
fromFile :: String -> IO (Maybe Graph)
fromFile path = 
        let stringToGraph x = fromString x

            graphValid      = MaybeT (fmap stringToGraph string) 
                            & (fmap adjacencyMatrixValid)
                            & runMaybeT
                            & (\x -> fromMaybe <$> pure False <*> x)
            string          = fmap init $ readFile path
            graph           = fmap stringToGraph string
        in graphValid >>= (\x -> if x then graph else return Nothing)

-- matrix exponent
(***) :: Num a => Matrix a -> Int -> Matrix a
matrix *** exponent = foldr1 multStd $ take exponent $ repeat matrix
