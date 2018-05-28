module Main where
import Matrix
import Lib
import System.Environment
import Data.Maybe
import qualified Data.Set as S
import Data.Function

main :: IO ()
main = do
        args <- getArgs
        -- read Graph
        maybeGraph <- Lib.fromFile (args!!0)
        if isJust maybeGraph 
            -- if maybeGraph is Some Graph there was no error parsing the file
            then do
                -- get the Graph out of its Maybe
                let graph = fromJust maybeGraph
                putStrLn "articulations:"
                putStrLn $ show $ Lib.articulations graph
                putStrLn "bridges:"
                putStrLn $ show $ Lib.bridges graph
                putStrLn "components:"
                let componentsSet = Lib.components graph
                let componentsList = map S.toList componentsSet
                putStrLn $ show $ componentsList
                let distance = Lib.distanceMatrix graph
                putStrLn "distance matrix:"
                putStrLn $ matrixToAsciiString $ distance
                putStrLn "eccentricities:"
                putStrLn $ show $ Lib.eccentricitiesFromDistance distance
                if length args > 1 && args!!1 == "--showForest"
                    then do
                        putStrLn "spanning forest:"
                        putStrLn $ spanningForestToString $ Lib.spanningForestFromComponents graph componentsSet
            -- if maybeGraph is Nothing, there was an error parsing the file
            else do putStrLn "Error: invalid input"

matrixToAsciiString :: Matrix Int -> String
matrixToAsciiString matrix = 
        let listList = toLists matrix
            listToString list = map show list
                              & foldr1 (\x y -> x ++ ";" ++ y) 
        in  foldr1 (\x y -> x ++ "\n" ++ y) $ map listToString listList
