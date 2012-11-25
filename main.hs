module Main where
import System.Environment

main :: IO ()
main = do
  a : b : _ <-  getArgs
  let c = read a + read b
  putStrLn ( show c )
