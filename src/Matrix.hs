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

-- |Main data structure for the Module.
-- 'Matrix' stores it's content inside a 'Map'
-- with (Int, Int) as a key.
-- number of rows and number of columns are also stored
-- in 'nrows' and 'ncols'
data Matrix a = Matrix { mmap  :: Map Index a
                       , ncols :: Int
                       , nrows :: Int
                       } deriving (Show)

-- key for the 'Map' used within 'Matrix'
type Index = (Int, Int)

instance Functor Matrix where
        fmap f m = m {mmap = Prelude.fmap f (mmap m)}

instance Foldable Matrix where 
        foldr f s c = Prelude.foldr f s (mmap c)

-- |'getElem' takes a 'x' and a 'y' coordinate and a 'Matrix' and returns
-- the element at the given coordinates.
getElem :: Int -> Int -> Matrix a -> a
getElem x y matrix = mmap matrix ! (x, y)

-- |'elementwise' takes a function and two matrices and zips the matrices
-- with the function
elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c 
elementwise operation matrixA matrixB = 
        let keys = toList (mmap matrixA) & fmap fst
        in  matrixA { mmap = fmap (\(x, y) -> let 
                                 valA      = getElem x y matrixA
                                 valB      = getElem x y matrixB
                                 valResult = operation valA valB
                                 index       = (x, y)
                             in  (index, valResult)) 
                             keys
                             & fromList }

-- |'setElem' takes an element, an 'Index' and a 'Matrix' and replaces the
-- element at the index.
setElem :: a -> Index -> Matrix a -> Matrix a
setElem val index matrix = 
        matrix {mmap = insert index val (mmap matrix)}

-- |returns a Matrix with 1s in the diagonal and 0s elsewhere.
identity :: Int -> Matrix Int
identity size = matrix size size (\(x, y) -> if x == y then 1 else 0)

-- |takes rows, columns and a constructor function and turns it into
-- a matrix.
matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix rows columns fn = 
                        -- generate list of all indices and call the
                        -- passed constructor function on all elemnts
                        -- then put the result into a tuple with the index
                        -- and create a new map from this list
        Matrix { mmap = fromList [((x, y), fn (x, y)) | x <- [1..rows], y <- [1..columns]]
               , nrows = rows
               , ncols = columns
               }

-- |constructs a 'Matrix' from a 'List' of 'List's
fromLists :: [[a]] -> Matrix a
fromLists lists = 
        matrix (length lists) --rows
               ((length $ lists !! 0)) --columns
               (\(x, y) -> lists !! (x - 1) !! (y - 1)) -- constructor function

-- |turns a 'Matrix' into a 'List' of 'List's
toLists :: Matrix a -> [[a]]
toLists matrix = 
        let indices = [1..ncols matrix]
        in  indices
            & fmap (\x -> fmap (\y -> getElem x y matrix) indices) 

-- |standard matrix multiplication
multStd :: Num a => Matrix a -> Matrix a -> Matrix a
multStd matrixA matrixB = matrix (nrows matrixA)
                                 (ncols matrixB)
                                 (\(x, y) -> 
                                         -- multiply each element from
                                         -- a row with each element of
                                         -- a column
                                    fmap (\i -> getElem i y matrixA * getElem x i matrixB) 
                                         [1..nrows matrixA] 
                                         -- and then sum the products
                                    & sum
                                 )
