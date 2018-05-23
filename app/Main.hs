module Main where
import Lib
import System.Environment
import Data.Maybe
import qualified Data.Set as S
import Data.Function
import Matrix

main :: IO ()
main = do
        args <- getArgs
        maybeGraph <- Lib.fromFile (args!!0)
        if isJust maybeGraph 
            then do
                let graph = fromJust maybeGraph
                putStrLn "articulations:"
                putStrLn $ show $ Lib.articulations graph
                putStrLn "bridges:"
                putStrLn $ show $ Lib.bridges graph
                putStrLn "components:"
                let componentsList = map S.toList $ Lib.components graph
                putStrLn $ show $ componentsList
                let distance = Lib.distanceMatrix graph
                putStrLn "distance matrix:"
                putStrLn $ matrixToAsciiString $ distance
                putStrLn "eccentricities:"
                putStrLn $ show $ Lib.eccentricitiesFromDistance distance
            else do putStrLn "Error: invalid input"

matrixToAsciiString :: Matrix Int -> String
matrixToAsciiString matrix = 
        let listList = toLists matrix
            listToString list = map show list
                              & foldr1 (\x y -> x ++ ";" ++ y) 
        in  foldr1 (\x y -> x ++ "\n" ++ y) $ map listToString listList
