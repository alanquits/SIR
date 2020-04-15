module Main where

import qualified SIR
import System.Environment
import qualified IO 

main :: IO ()
main = do
    args <- getArgs
    maybe_model <- IO.fromFile (args !! 0) -- will not emit a useful error message on failure
    case maybe_model of
        Nothing -> putStrLn "Unable to parse input file"
        Just model -> SIR.run model
    