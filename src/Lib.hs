module Lib
    ( fromFile
    , distanceMatrix
    , eccentricitiesFromDistance
    , bridges
    , articulations
    , components
    , dfs
    ) where
import Data.List.Split
import Data.Matrix
import Data.Function
import Data.Maybe
import qualified Data.Set as S
import Control.Monad.Trans.Maybe
import System.Random

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

distanceMatrix :: Graph -> Graph
distanceMatrix graph = distanceMatrix' graph distances 1
        where distances = graph 
                          & fmap (\x -> if x == 0 then -1 else x)
                          & (\x -> elementwise (*) (fmap (\y -> if y == 1 then 0 else 1) 
                                                                  $ identity 
                                                                  $ nrows graph) 
                                           x)

distanceMatrix' :: Graph -> Graph -> Int -> Graph
distanceMatrix' graph distances step =
        let size       = nrows graph
            graphPowN  = graph *** step
            distances' = matrix size size
                           (\(x, y) ->
                             if
                               getElem x y graphPowN /= 0 &&
                               getElem x y distances == -1
                             then
                               step
                             else
                               getElem x y distances)
        in  if step == size then distances else distanceMatrix' graph distances' (step + 1)

eccentricitiesFromDistance :: Matrix Int -> [Int]
eccentricitiesFromDistance distance = map maximum $ toLists distance

removeEdge :: Graph -> (Int, Int) -> Graph
removeEdge graph (vertexA, vertexB) = setElem 0 (vertexA + 1, vertexB + 1) 
                                        $ setElem 0 (vertexB + 1, vertexA + 1) graph

dfs' :: Graph -> Int -> [Int] -> S.Set Int
dfs' graph startVertex discovered = 
        let adjacent              = adjacentVertices graph startVertex
            graphWithRemovedEdges :: Graph
            graphWithRemovedEdges = foldr (\v g -> removeEdge g (v, startVertex)) graph adjacent
            flatten = foldr1 (++)
        in  if length adjacent == 0 
                then S.fromList discovered 
                else S.fromList (startVertex : discovered ++ (flatten $ map (\x -> S.toList (dfs' graphWithRemovedEdges x (adjacent++discovered))) adjacent))

dfs :: Graph -> Int -> S.Set Int
dfs graph startVertex = dfs' graph startVertex [startVertex]

adjacentVertices :: Graph -> Int -> [Int]
adjacentVertices graph vertex = toLists graph !! vertex
                              & zip [0..]
                              & filter (\(index, connection) -> connection == 1)
                              & map fst

isBridge ::  Graph -> (Int, Int) -> Bool
isBridge graph (vertexA, vertexB) = 
        not $ vertexB `S.member` (dfs withRemovedEdge vertexA)
          where withRemovedEdge = removeEdge graph (vertexA, vertexB)

edges :: Graph -> [(Int, Int)]
edges graph = [(x, y) | x <- [1..nrows graph - 1], y <- [1..ncols graph - 1]]
            & filter (\(x, y) -> getElem (x + 1) (y + 1) graph == 1)

bridges :: Graph -> [(Int, Int)]
bridges graph = edges graph 
              & filter (isBridge graph)

removeAdjacentEdges :: Graph -> Int -> Graph
removeAdjacentEdges graph vertex = adjacentVertices graph vertex
                                 & foldr (\v g -> removeEdge g (vertex, v)) graph 

adjacentEdges :: Graph -> Int -> [(Int, Int)]
adjacentEdges graph vertex = adjacentVertices graph vertex `zip` repeat vertex

isArticulation :: Graph -> Int -> Bool
isArticulation graph vertex
        | (length adjacent) < 2 = False
        | otherwise =  all (\x -> not $ S.member x dfsFromFirst) rest
            where adjacent        = adjacentVertices graph vertex
                  (first:rest)    = adjacent
                  withoutAdjacent = removeAdjacentEdges graph vertex
                  dfsFromFirst    = dfs withoutAdjacent first

articulations ::  Graph -> [Int]
articulations graph = [0..nrows graph - 1]
                    & filter (isArticulation graph)

components :: Graph -> [S.Set Int]
components graph = components' graph S.empty

components' :: Graph -> S.Set Int -> [S.Set Int]
components' graph alreadyFound = if S.null notFound 
                                     then [] 
                                     else thisComponent:
                                          (components' graph $ 
                                            S.union alreadyFound thisComponent)
        where notFound      = S.fromList [0..nrows graph - 1] S.\\ alreadyFound  
              startVal      = S.findMin notFound
              thisComponent = dfs graph startVal
