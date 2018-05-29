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
treeToSet (Tree vertex set) = 
        -- recursively call treeToSet on each subtree in the set and
        -- unionize them 
        S.map treeToSet set & S.toList & (S.singleton vertex:) & S.unions

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
          -- list with all vertices
          allVertices       = [1..nrows matrix]
          -- all entries in the matrix are ones or zeroes
          allOnesOrZeroes = all (\x -> x == 0 || x == 1) matrix
          isQuadratic     = nrows matrix == ncols matrix
          hasLoop         = any (\x -> getElem x x matrix /= 0) allVertices
          isDirected      = any (\(x, y) -> getElem x y matrix /= getElem y x matrix)
                            [(x, y) | x <- allVertices, y <- allVertices]

-- 'fromString' takes a String and tries to read a matrix from it.
-- if the string contains valid csv with only '1's and '0's as entries
-- and ';'s as seperation characters.
-- if the String does not contain valid csv nothing is returned, else Some
-- Matrix is returned
fromString :: String -> Maybe (Matrix Int)
fromString string = 
        -- remove spaces
        filter (/=' ') string 
        -- split on newlines so it becomes a list of lines
      & splitOn "\n" 
        -- split the lines on semicolons so it becomes a list of lists
      & map (splitOn ";") 
        -- turn [[String]] into [Maybe [Int]] by calling readBinaryInt and
        -- sequencing it
      & map (\x -> sequence $ map readBinaryInt x) 
        -- turn [Maybe[Int]] into Maybe[[Int]]
      & sequence 
        -- turn it into Maybe (Matrix Int)
      & fmap fromLists 

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
            -- check if graph is valid
                              -- create MonadTransformer of type 
                              -- MaybeT IO Graph
        let graphValid      = MaybeT (fmap fromString string) 
                              -- check if the graph is valid
                            & (fmap adjacencyMatrixValid)
                              -- turn it back into IO (Maybe Boolean)
                            & runMaybeT
                              -- if its Nothing it gets turned into False
                              -- else Maybe True gets turned into True and
                              -- Maybe False gets turned into False
                            & (\x -> fromMaybe <$> pure False <*> x)
            -- the file string but the last element (EOF) gets droped
            string          = fmap init $ readFile path
            -- calculate the graph from the string
            graph           = fmap fromString string
        -- if the graph is valid IO Some Graph is returned, else IO Nothing
        in graphValid >>= (\x -> if x then graph else return Nothing)

-- matrix exponent
(***) :: Num a => Matrix a -> Int -> Matrix a
matrix *** exponent = 
        -- multiply the matrix with itself exponent times
        foldr1 multStd $ 
            -- this is an list with the matrix in it with a length of
            -- exponent
            take exponent $ repeat matrix 

-- |'distanceMatrix' takes a 'Graph' and returns the distance Matrix of the
-- Graph
distanceMatrix :: Graph -> Matrix Int
distanceMatrix graph = distanceMatrix' graph initial 1
        where initial = graph 
                      -- turn all zeroes into -1s
                      & fmap (\x -> if x == 0 then -1 else x)
                      -- turn the vertical into zeroes by multipliyng it
                      -- with an `inverted` identity matrix
                      & (\x -> elementwise (*) (fmap (\y -> if y == 1 then 0 else 1) 
                                                              $ identity 
                                                              $ nrows graph) 
                                           x)

-- helper Function for 'distanceMatrix'
distanceMatrix' :: Graph -> Graph -> Int -> Graph
distanceMatrix' graph distances step =
        let size       = nrows graph
            graphPowN  = graph *** step
            -- distances where all elements == -1 and elements where the
            -- matrix to the power of the step is not 0 are replaced with
            -- step
            distances' = matrix size size
                           (\(x, y) ->
                             if
                               getElem x y graphPowN /= 0 &&
                               getElem x y distances == -1
                             then
                               step
                             else
                               getElem x y distances)
        -- recursive call if the calculation is not finished
        in  if step == size then distances else distanceMatrix' graph distances' (step + 1)

-- |'eccentricitiesFromDistance' takes a Distance Matrix and returns
-- a 'List' of 'Int' where the entry at position 0 is the eccentricity of
-- vertex 0, the entry at position 1 is the eccentrcity of verte 1, ...
eccentricitiesFromDistance :: Matrix Int -> [Int]
                                      -- take a list of distances and take
                                      -- the maximum
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
            -- if there are no(more) vertices to search from, an empty set
            -- is returned
            then S.empty
            -- else a set of start vertices and vertices returned by
            -- a recursive call are being returned
            else startVertices `S.union` (dfs graphWithoutStartVertices adjacentStartVertices)
        where graphWithoutStartVertices = foldr (flip removeEdge) graph incidentStartEdges
              -- all edges incident to the start vertices
              incidentStartEdges        = S.map (S.fromList.(incidentEdges graph)) 
                                                startVertices
                                          & S.toList
                                          & S.unions
              -- all edges adjacent to the start vertices
              adjacentStartVertices     = S.map (S.fromList.(adjacentVertices graph)) 
                                                startVertices
                                          & S.toList
                                          & S.unions


-- takes a 'Graph' and a 'Vertex' and returns a 'List' of adjacent
-- 'Vertex's
adjacentVertices :: Graph -> Vertex -> [Vertex]
adjacentVertices graph vertex = 
        -- take a binary list of adjacent vertices
        toLists graph !! vertex
        -- zip it with the indices
      & zip [0..]
        -- filter the ones where the value is 1
      & filter (\(index, connection) -> connection == 1)
        -- take only the indices
      & map fst

-- Takes a 'Graph' and a 'Vertex' and return all Edges, adjacent to the
-- Vertex
incidentEdges :: Graph -> Vertex -> [Edge]
incidentEdges graph vertex = 
        -- take adjacent Vertices and zip them with a list of the start
        -- Vertex
        adjacentVertices graph vertex `zip` repeat vertex

-- Takes a 'Graph' and a 'Edge' and returns 'True' if the Edges is
-- a Bridge.
isBridge ::  Graph -> Edge -> Bool
isBridge graph (vertexA, vertexB) = 
        -- do a dfs from vertex A where the Edge from Vertex A to 
        -- Vertex B is removed and see if Vertex B is still found
        not $ vertexB `S.member` (dfs withRemovedEdge (S.singleton vertexA))
          where withRemovedEdge = removeEdge graph (vertexA, vertexB)

-- returns all 'Edge's of a given 'Graph'
edges :: Graph -> [Edge]
edges graph = 
        -- list of all possible edges
        [(x, y) | x <- [0..nrows graph - 1], y <- [0..ncols graph - 1]]
        -- filter them by getting just the edges that are actually in the
        -- graph
      & filter (\(x, y) -> getElem (x + 1) (y + 1) graph == 1)

-- |returns all bridges of a given 'Graph'
bridges :: Graph -> [Edge]
bridges graph = edges graph 
              & filter (\(x, y) -> x < y)
              & filter (isBridge graph)

-- Takes a 'Graph' and a 'Vertex' and returns the given Graph where all
-- Edges adjacent to the given Vertex are removed
removeIncidentEdges :: Graph -> Vertex -> Graph
removeIncidentEdges graph vertex = 
        adjacentVertices graph vertex
      & foldr (\v g -> removeEdge g (vertex, v)) graph 

-- Takes a 'Graph' and a 'Vertex' and return True if the Vertex is an
-- articulation.
isArticulation :: Graph -> Vertex -> Bool
isArticulation graph vertex
        -- if there are less than two adjacent vertices it can't be an
        -- articulation
        | (length adjacent) < 2 = False
        -- do a dfs from the first adjacent vertex, if all of the rest of
        -- the adjacent vertices aren't reachable, its an articulation
        | otherwise =  any (\x -> not $ S.member x dfsFromFirst) rest
            where adjacent        = adjacentVertices graph vertex
                  (first:rest)    = adjacent
                  -- graph without adjacent edges
                  withoutAdjacent = removeIncidentEdges graph vertex
                  -- dfs results from the first adjacent vertex
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
          -- if the Set of vertices that are not already found is empty, an
          -- empty list is returned
          then [] 
          -- current component is added to recursive call for rest of
          -- components
          else thisComponent:
               (components' graph $ 
                 S.union alreadyFound thisComponent)
                  -- vertices that are not found yet
            where notFound      = S.fromList [0..nrows graph - 1] S.\\ alreadyFound  
                  -- value to start recursive call of components'
                  startVal      = S.findMin notFound
                  -- calculate next component
                  thisComponent = dfs graph (S.singleton startVal)

-- calculate spanning tree from 'Graph' and start 'Vertex'
spanningTree :: Graph -> Vertex -> Tree
spanningTree graph startVertex =
            -- vertices adjacent to the startVertex
        let adjacent                = adjacentVertices graph startVertex
            -- Graph without the Edges indicent to the startVertex
            graphWithoutStartVertex = removeIncidentEdges graph startVertex
            -- recursive call for all incident Edges
        in  foldr (\vertex (graph, tree) ->
                           -- sub tree for current incident edge
                       let subTree       = spanningTree graph vertex
                           -- tree with subtree appended
                           newTree       = appendToTree tree subTree
                           -- vertices in the subtree
                           foundVertices = treeToSet subTree
                           -- graph without the edges incident to any
                           -- subtree vertex
                           graphWithout  = foldl removeIncidentEdges graph foundVertices
                       in  if vertex `S.member` (treeToSet tree)
                                   -- if the vertex is already found, the values
                                   -- are passed without modification
                              then (graph, tree)
                                   -- if the vertex is not already found,
                                   -- the graph wihtout the subtree and the
                                   -- tree with the subtree appended are
                                   -- passed on
                              else (graphWithout, newTree)
                  )
                  (graphWithoutStartVertex, singletonTree startVertex)
                  adjacent
              -- take only the new 'Tree' and drop the 'Graph'
            & snd


-- |take a 'Graph' and Components and return a list of 'Tree's
spanningForestFromComponents :: Graph -> [S.Set Vertex] -> [Tree]
spanningForestFromComponents graph components = 
        map (\x -> spanningTree graph $ S.findMin x) components

-- |take a list of 'Tree's and turn them into a somewhat readable 'String'
spanningForestToString :: [Tree] -> String
spanningForestToString trees = map spanningTreeToString trees & foldr1 (\x y -> x ++ "\n" ++ y)


-- take a Tree' and turn it into a somewhat readable 'String'
spanningTreeToString :: Tree -> String
spanningTreeToString tree = spanningTreeToString' 0 tree

-- helper function for 'spanningTreeToString'
spanningTreeToString' :: Int -> Tree -> String
spanningTreeToString' indent (Tree vertex set) = 
           -- indentation spaces
           indentSpaces
           -- add the vertex number
        ++ show vertex 
           -- add the string from the recursive call to the set of subtrees
        ++ setString
           where indentSpaces = if indent > 0 
                                  then foldr1 (++) $ take indent (repeat "   ")
                                  else ""
                 setString    = if S.null set 
                                  then ""
                                       -- put newlines between the elements
                                       -- of the set
                                  else "\n" ++ (foldr1 (\x y -> x++"\n"++y) $ 
                                    S.map (spanningTreeToString' (indent + 1)) set)
