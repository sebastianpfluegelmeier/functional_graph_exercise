module Lib
    ( Graph
    , Vertex
    , Edge
    , adjacencyMatrixValid
    , fromFile
    , distanceMatrix
    , eccentricitiesFromDistance
    , bridges
    , articulations
    , components
    , dfs
    , spanningForestFromComponents
    , spanningForestToString
    ) where
import Matrix 
import Data.List.Split
import Data.Function
import Data.Maybe
import qualified Data.Set as S
import Control.Monad.Trans.Maybe
import System.Random
import Debug.Trace

-- |'Graph' is the main Datatype for this Module.
-- Its simply a Matrix of 'Int's where each entry should bei @1@ or @0@
type Graph = Matrix Vertex

-- |'Vertex' represents the index of a Vertex
type Vertex = Int

-- |'Edge' represents a Edge in a graph
type Edge = (Vertex, Vertex)

-- |Tree type for representing spanning Trees
data Tree = Tree Vertex (S.Set Tree) deriving (Eq, Show)

instance Ord Tree where
        compare (Tree a _) (Tree b _) = compare a b


-- constructs a 'Tree' with only one 'Vertex'
singletonTree :: Vertex -> Tree
singletonTree vertex = Tree vertex S.empty

-- take two 'Tree's and append the second one to the root Node of the first
-- one.
appendToTree :: Tree -> Tree -> Tree
appendToTree (Tree vertex branches) treeToAppend = Tree vertex (S.insert treeToAppend branches)

treeToSet :: Tree -> S.Set Int
treeToSet (Tree vertex set) = S.map treeToSet set & S.toList & (S.singleton vertex:) & S.unions

-- |'adjacencyMatrixValid' takes a Matrix of Ints returns true if the
-- Matrix follows this criteria:
--
--     * matrix is quadratic - neccesarry for adjacency matrix 
--
--     * all diagonal entries are zero - adjacency matrix has no loops
--
--     * matrix is symetric - adjacency matrix is directed
--
--     * all entries are one or zero - values other than one or zero are
--       meaningless in a adjacency matrix
adjacencyMatrixValid :: Matrix Int -> Bool
adjacencyMatrixValid matrix = isQuadratic && not hasLoop && not isDirected && allOnesOrZeroes
    where
          sizeRange       = [1..nrows matrix]
          allOnesOrZeroes = all (\x -> x == 0 || x == 1) matrix
          isQuadratic     = nrows matrix == ncols matrix
          hasLoop         = any (\x -> getElem x x matrix /= 0) sizeRange
          isDirected      = any (\(x, y) -> getElem x y matrix /= getElem y x matrix)
                            [(x, y) | x <- sizeRange, y <- sizeRange]

-- 'fromString' takes a String and tries to read a matrix from it.
-- if the string contains valid csv with only '1's and '0's as entries
-- and ';'s as seperation characters.
-- if the String does not contain valid csv nothing is returned, else Some
-- Matrix is returned
fromString :: String -> Maybe (Matrix Int)
fromString string = fmap fromLists $ sequence $ map (\x -> sequence $ map readBinaryInt x) $ map (splitOn ";") $ splitOn "\n" $ filter (/=' ') string

-- 'readBinaryInt' tries to read 0 or 1 from a String. if the String is not
-- "0" or "1", Nothing is returned
readBinaryInt :: String -> Maybe Int
readBinaryInt string
    | string == "1" = Just 1
    | string == "0" = Just 0
    | otherwise     = Nothing

-- |'fromFile' reads a Graph from a File 
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

-- |'distanceMatrix' takes a 'Graph' and returns the distance Matrix of the
-- Graph
distanceMatrix :: Graph -> Matrix Int
distanceMatrix graph = distanceMatrix' graph distances 1
        where distances = graph 
                          & fmap (\x -> if x == 0 then -1 else x)
                          & (\x -> elementwise (*) (fmap (\y -> if y == 1 then 0 else 1) 
                                                                  $ identity 
                                                                  $ nrows graph) 
                                           x)

-- helper Function for 'distanceMatrix'
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

-- |'eccentricitiesFromDistance' takes a Distance Matrix and returns
-- a 'List' of 'Int' where the entry at position 0 is the eccentricity of
-- vertex 0, the entry at position 1 is the eccentrcity of verte 1, ...
eccentricitiesFromDistance :: Matrix Int -> [Int]
eccentricitiesFromDistance distance = toLists distance
                                    & map maximum
                                    & zipWith (\x y -> if y == -1 then y else x)
                                              (toLists distance & map minimum)

-- take a 'Graph' and an 'Edge', return a Graph without the given Edge
removeEdge :: Graph -> Edge -> Graph
removeEdge graph (vertexA, vertexB) = setElem 0 (vertexA + 1, vertexB + 1) 
                                    $ setElem 0 (vertexB + 1, vertexA + 1) graph

-- takes a 'Graph' and a Set of start 'Vertex's, return a Set of 'Vertex's
-- containing all found Vertices in a depth first search.
dfs :: Graph -> S.Set Vertex -> S.Set Vertex
dfs graph startVertices = 
        if S.null startVertices 
            then S.empty
            else startVertices `S.union` (dfs graphWithoutStartVertices adjacentStartVertices)
        where graphWithoutStartVertices = foldr (flip removeEdge) graph adjacentStartEdges

              adjacentStartEdges        = S.map (S.fromList.(adjacentEdges graph)) 
                                                startVertices
                                          & S.toList
                                          & S.unions
              adjacentStartVertices     = S.map (S.fromList.(adjacentVertices graph)) 
                                                startVertices
                                          & S.toList
                                          & S.unions


-- takes a 'Graph' and a 'Vertex' and returns a 'List' of adjacent
-- 'Vertex's
adjacentVertices :: Graph -> Vertex -> [Vertex]
adjacentVertices graph vertex = toLists graph !! vertex
                              & zip [0..]
                              & filter (\(index, connection) -> connection == 1)
                              & map fst

-- Takes a 'Graph' and a 'Vertex' and return all Edges, adjacent to the
-- Vertex
adjacentEdges :: Graph -> Vertex -> [Edge]
adjacentEdges graph vertex = adjacentVertices graph vertex `zip` repeat vertex

-- Takes a 'Graph' and a 'Edge' and returns 'True' if the Edges is
-- a Bridge.
isBridge ::  Graph -> Edge -> Bool
isBridge graph (vertexA, vertexB) = 
        not $ vertexB `S.member` (dfs withRemovedEdge (S.singleton vertexA))
          where withRemovedEdge = removeEdge graph (vertexA, vertexB)

-- returns all 'Edge's of a given 'Graph'
edges :: Graph -> [Edge]
edges graph = [(x, y) | x <- [1..nrows graph - 1], y <- [1..ncols graph - 1]]
            & filter (\(x, y) -> getElem (x + 1) (y + 1) graph == 1)

-- |returns all bridges of a given 'Graph'
bridges :: Graph -> [Edge]
bridges graph = edges graph 
              & filter (\(x, y) -> x < y)
              & filter (isBridge graph)

-- Takes a 'Graph' and a 'Vertex' and returns the given Graph where all
-- Edges adjacent to the given Vertex are removed
removeIncidentEdges :: Graph -> Vertex -> Graph
removeIncidentEdges graph vertex = adjacentVertices graph vertex
                                 & foldr (\v g -> removeEdge g (vertex, v)) graph 

-- Takes a 'Graph' and a 'Vertex' and return True if the Vertex is an
-- articulation.
isArticulation :: Graph -> Vertex -> Bool
isArticulation graph vertex
        | (length adjacent) < 2 = False
        | otherwise =  all (\x -> not $ S.member x dfsFromFirst) rest
            where adjacent        = adjacentVertices graph vertex
                  (first:rest)    = adjacent
                  withoutAdjacent = removeIncidentEdges graph vertex
                  dfsFromFirst    = dfs withoutAdjacent (S.singleton first)

-- |returns a 'List' of all articulations of a 'Graph'
articulations ::  Graph -> [Vertex]
articulations graph = [0..nrows graph - 1]
                    & filter (isArticulation graph)

-- |returns a list of all components of a 'Graph' where a component is
-- represented as a list of Sets of Vertices
components :: Graph -> [S.Set Vertex]
components graph = components' graph S.empty

-- helper function for components
components' :: Graph -> S.Set Vertex -> [S.Set Vertex]
components' graph alreadyFound = 
        if S.null notFound 
          then [] 
          else thisComponent:
               (components' graph $ 
                 S.union alreadyFound thisComponent)
            where notFound      = S.fromList [0..nrows graph - 1] S.\\ alreadyFound  
                  startVal      = S.findMin notFound
                  thisComponent = dfs graph (S.singleton startVal)

spanningTree :: Graph -> Vertex -> Tree
spanningTree graph startVertex =
        let adjacent                = adjacentVertices graph startVertex
            graphWithoutStartVertex = removeIncidentEdges graph startVertex
        in  foldr (\vertex (graph, tree) ->
                       let subTree       = spanningTree graph vertex
                           newTree       = appendToTree tree subTree
                           foundVertices = treeToSet subTree
                           graphWithout  = foldl removeIncidentEdges graph foundVertices
                       in  if vertex `S.member` (treeToSet tree)
                              then (graph, tree)
                              else (graphWithout, newTree)
                  )
                  (graphWithoutStartVertex, singletonTree startVertex)
                  adjacent
            & snd


spanningForestFromComponents :: Graph -> [S.Set Vertex] -> [Tree]
spanningForestFromComponents graph components = 
        map (\x -> spanningTree graph $ S.findMin x) components

spanningForestToString :: [Tree] -> String
spanningForestToString trees = map spanningTreeToString trees & foldr1 (\x y -> x ++ "\n" ++ y)

spanningTreeToString :: Tree -> String
spanningTreeToString tree = spanningTreeToString' 0 tree

spanningTreeToString' :: Int -> Tree -> String
spanningTreeToString' indent (Tree vertex set) = 
           indentSpaces
        ++ show vertex 
        ++ setString
          where indentSpaces = if indent > 0 
                                 then foldr1 (++) $ take indent (repeat "   ")
                                 else ""
                setString    = if S.null set 
                                 then ""
                                 else "\n" ++ (foldr1 (\x y -> x++"\n"++y) $ 
                                   S.map (spanningTreeToString' (indent + 1)) set)

matrixToAsciiString :: Matrix Int -> String
matrixToAsciiString matrix = 
        let listList = toLists matrix
            listToString list = map show list
                              & foldr1 (\x y -> x ++ ";" ++ y) 
        in  foldr1 (\x y -> x ++ "\n" ++ y) $ map listToString listList
