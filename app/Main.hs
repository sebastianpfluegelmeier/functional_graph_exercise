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
                    else return ()
            -- if maybeGraph is Nothing, there was an error parsing the file
            else do 
                putStrLn "Error: invalid input"

matrixToAsciiString :: Matrix Int -> String
matrixToAsciiString matrix = 
            -- matrix turned into a [[Int]]
        let listList = toLists matrix
            -- turn a list into a string by putting a semicolon between its elements
            listToString list = map show list
                              -- replace "-1"s with "-"s
                              & map (\x ->  
                                      if x == "-1" 
                                          then "-" 
                                          else x
                                     )
                              & foldr1 (\x y -> x ++ ";" ++ y) 
            -- turn the list of lists into a a big string by applying
            -- listToString to the inner lists and folding the outer list
            -- by appending its elements while putting newlines between them
        in  foldr1 (\x y -> x ++ "\n" ++ y) $ map listToString listList
